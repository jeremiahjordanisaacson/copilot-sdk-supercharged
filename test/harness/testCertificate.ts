/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

/**
 * Minimal X.509 fixture encoding, not a general-purpose certificate library.
 * All cryptography and certificate parsing use Node's native implementation.
 */
import {
  createHash,
  generateKeyPairSync,
  randomBytes,
  sign,
  X509Certificate,
  type KeyObject,
} from "node:crypto";
import { isIP } from "node:net";
import { promisify } from "node:util";

export interface TestCertificate {
  certPem: string;
  keyPem: string;
  caCert: X509Certificate;
  caKey: KeyObject;
  subjectDer: Buffer;
  keyIdentifier: Buffer;
}

interface CertificateOptions {
  commonName: string;
  organization?: string;
  issuer?: TestCertificate;
  keys?: ReturnType<typeof generateCertificateKeys>;
  serialNumber?: string;
  notBefore: Date;
  notAfter: Date;
  ca?: boolean;
  pathLength?: number;
  purpose?: "server" | "client";
  hosts?: string[];
  issuerUrl?: string;
}

// DER content octets of the fixed OIDs used by these fixtures.
const oids = {
  commonName: "550403",
  organizationName: "55040a",
  basicConstraints: "551d13",
  keyUsage: "551d0f",
  extKeyUsage: "551d25",
  serverAuth: "2b06010505070301",
  clientAuth: "2b06010505070302",
  subjectAltName: "551d11",
  authorityKeyIdentifier: "551d23",
  subjectKeyIdentifier: "551d0e",
  authorityInfoAccess: "2b06010505070101",
  caIssuers: "2b06010505073002",
  sha256WithRSAEncryption: "2a864886f70d01010b",
};

export function generateCertificateKeys() {
  return generateKeyPairSync("rsa", { modulusLength: 2048, publicExponent: 65537 });
}

// Seed once off-thread; issuing a leaf during a TLS handshake needs uniqueness,
// not another synchronous entropy request.
let serialNumber = BigInt(`0x${(await promisify(randomBytes)(15)).toString("hex")}`);
export function nextCertificateSerial(): string {
  return `01${(serialNumber++).toString(16).padStart(30, "0")}`;
}

export function shortCertificateValidity(days: number): { notBefore: Date; notAfter: Date } {
  const notBefore = new Date();
  notBefore.setMinutes(notBefore.getMinutes() - 1);
  const notAfter = new Date(notBefore);
  notAfter.setDate(notAfter.getDate() + days);
  return { notBefore, notAfter };
}

export function createTestCertificate(options: CertificateOptions): TestCertificate {
  const keys = options.keys ?? generateCertificateKeys();
  const subjectDer = sequence(
    nameAttribute(oids.commonName, options.commonName),
    ...(options.organization ? [nameAttribute(oids.organizationName, options.organization)] : []),
  );
  // RFC 5280 method (1): SHA-1 identifies the RSA public key; it is not used
  // for a signature or any security decision.
  const keyIdentifier = createHash("sha1")
    .update(keys.publicKey.export({ type: "pkcs1", format: "der" }))
    .digest();
  const extensions = [
    extension(
      oids.basicConstraints,
      sequence(
        ...(options.ca ? [der(0x01, Buffer.from([0xff]))] : []),
        ...(options.pathLength === undefined
          ? []
          : [integer(options.pathLength.toString(16).padStart(2, "0"))]),
      ),
      true,
    ),
    extension(
      oids.keyUsage,
      // keyCertSign + cRLSign for CAs; digitalSignature, plus keyEncipherment
      // for servers. The first byte is the BIT STRING's unused-bit count.
      der(
        0x03,
        Buffer.from(options.ca ? [1, 6] : options.purpose === "client" ? [7, 0x80] : [5, 0xa0]),
      ),
      true,
    ),
  ];
  if (options.purpose) {
    extensions.push(
      extension(
        oids.extKeyUsage,
        sequence(oid(options.purpose === "server" ? oids.serverAuth : oids.clientAuth)),
      ),
    );
  }
  if (options.hosts) {
    extensions.push(extension(oids.subjectAltName, sequence(...options.hosts.map(subjectAltName))));
  }
  if (options.issuer) {
    extensions.push(
      extension(oids.authorityKeyIdentifier, sequence(der(0x80, options.issuer.keyIdentifier))),
    );
  }
  extensions.push(extension(oids.subjectKeyIdentifier, der(0x04, keyIdentifier)));
  if (options.issuerUrl) {
    extensions.push(
      extension(
        oids.authorityInfoAccess,
        sequence(sequence(oid(oids.caIssuers), der(0x86, Buffer.from(options.issuerUrl, "ascii")))),
      ),
    );
  }

  const algorithm = sequence(oid(oids.sha256WithRSAEncryption), der(0x05, Buffer.alloc(0)));
  const tbs = sequence(
    der(0xa0, integer("02")),
    integer(options.serialNumber ?? nextCertificateSerial()),
    algorithm,
    options.issuer?.subjectDer ?? subjectDer,
    sequence(time(options.notBefore), time(options.notAfter)),
    subjectDer,
    keys.publicKey.export({ type: "spki", format: "der" }),
    der(0xa3, sequence(...extensions)),
  );
  const signature = sign("sha256", tbs, options.issuer?.caKey ?? keys.privateKey);
  const certificate = sequence(
    tbs,
    algorithm,
    der(0x03, Buffer.concat([Buffer.from([0]), signature])),
  );
  const certPem = `-----BEGIN CERTIFICATE-----\n${certificate
    .toString("base64")
    .match(/.{1,64}/g)!
    .join("\n")}\n-----END CERTIFICATE-----\n`;
  return {
    certPem,
    keyPem: keys.privateKey.export({ type: "pkcs1", format: "pem" }).toString(),
    caCert: new X509Certificate(certificate),
    caKey: keys.privateKey,
    subjectDer,
    keyIdentifier,
  };
}

function der(tag: number, value: Buffer): Buffer {
  const lengthHex = value.length.toString(16);
  const longLength = Buffer.from(
    lengthHex.padStart(lengthHex.length + (lengthHex.length % 2), "0"),
    "hex",
  );
  const length =
    value.length < 128
      ? Buffer.from([value.length])
      : Buffer.concat([Buffer.from([0x80 | longLength.length]), longLength]);
  return Buffer.concat([Buffer.from([tag]), length, value]);
}

function sequence(...values: Buffer[]): Buffer {
  return der(0x30, Buffer.concat(values));
}

function oid(hex: string): Buffer {
  return der(0x06, Buffer.from(hex, "hex"));
}

function integer(hex: string): Buffer {
  if (!/^[0-9a-f]+$/i.test(hex)) {
    throw new Error(`Invalid certificate integer: ${hex}`);
  }
  const padded = hex.padStart(hex.length + (hex.length % 2), "0");
  const bytes = Buffer.from(padded.replace(/^(?:00)+(?=..)/, ""), "hex");
  return der(0x02, bytes[0] & 0x80 ? Buffer.concat([Buffer.from([0]), bytes]) : bytes);
}

function nameAttribute(id: string, value: string): Buffer {
  return der(0x31, sequence(oid(id), der(0x0c, Buffer.from(value, "utf8"))));
}

function extension(id: string, value: Buffer, critical = false): Buffer {
  return sequence(oid(id), ...(critical ? [der(0x01, Buffer.from([0xff]))] : []), der(0x04, value));
}

function time(date: Date): Buffer {
  const value = date
    .toISOString()
    .replace(/[-:]/g, "")
    .replace("T", "")
    .replace(/\.\d{3}Z$/, "Z");
  const utc = date.getUTCFullYear() >= 1950 && date.getUTCFullYear() < 2050;
  return der(utc ? 0x17 : 0x18, Buffer.from(utc ? value.slice(2) : value, "ascii"));
}

function subjectAltName(hostname: string): Buffer {
  switch (isIP(hostname)) {
    case 4:
      return der(0x87, Buffer.from(hostname.split(".").map(Number)));
    case 6: {
      // WHATWG URL normalizes embedded IPv4 and IPv6 groups. Zone identifiers
      // are not certificate IP addresses and are rejected by URL.
      const normalized = new URL(`http://[${hostname}]/`).hostname.slice(1, -1);
      const [left, right] = normalized.split("::").map((part) => (part ? part.split(":") : []));
      const groups = right
        ? [...left, ...Array<string>(8 - left.length - right.length).fill("0"), ...right]
        : left;
      const bytes = Buffer.alloc(16);
      groups.forEach((group, index) => bytes.writeUInt16BE(Number.parseInt(group, 16), index * 2));
      return der(0x87, bytes);
    }
    default:
      return der(0x82, Buffer.from(hostname, "ascii"));
  }
}
