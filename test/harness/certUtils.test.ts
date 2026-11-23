/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import { createHash, createPrivateKey, X509Certificate } from "node:crypto";
import { describe, expect, test, vi } from "vitest";
import { createIdentityForHost, generateCA } from "./certUtils.js";
import { createTestCertificate, generateCertificateKeys } from "./testCertificate.js";

describe("certificate fixtures", () => {
  test("creates a self-signed RSA/SHA256 CA with the required critical extensions", () => {
    const ca = generateCA();
    const cert = new X509Certificate(ca.certPem);
    expect(cert.verify(cert.publicKey)).toBe(true);
    expect(cert.checkPrivateKey(createPrivateKey(ca.keyPem))).toBe(true);
    expect(cert.ca).toBe(true);
    expect(cert.serialNumber).toBe("01");
    expect(cert.subject).toBe("CN=SDK E2E Test CA\nO=Copilot SDK Tests");
    expect(cert.issuer).toBe(cert.subject);
    expect(cert.publicKey.asymmetricKeyDetails).toMatchObject({
      modulusLength: 2048,
      publicExponent: 65537n,
    });
    expect(cert.raw.toString("hex").match(/300d06092a864886f70d01010b0500/g)).toHaveLength(2);
    expect(cert.raw.includes(Buffer.from("0603551d130101ff040530030101ff", "hex"))).toBe(true);
    expect(cert.raw.includes(Buffer.from("0603551d0f0101ff040403020106", "hex"))).toBe(true);
    const keyId = createHash("sha1")
      .update(cert.publicKey.export({ format: "der", type: "pkcs1" }))
      .digest("hex");
    expect(cert.raw.includes(Buffer.from(`0603551d0e04160414${keyId}`, "hex"))).toBe(true);
    const expectedExpiry = new Date(cert.validFrom);
    expectedExpiry.setFullYear(expectedExpiry.getFullYear() + 1);
    expect(new Date(cert.validTo)).toEqual(expectedExpiry);
  });

  test.each([
    "server.example.com",
    "192.0.2.1",
    "::1",
    "::",
    "2001:db8::1",
    "2001:db8:1:2:3:4:5:6",
    "::ffff:192.0.2.1",
  ])("issues a verifiable server identity for %s", (hostname) => {
    const ca = generateCA();
    const identity = createIdentityForHost(hostname, ca);
    const cert = new X509Certificate(identity.certPem);
    expect(cert.verify(ca.caCert.publicKey)).toBe(true);
    expect(cert.checkIssued(ca.caCert)).toBe(true);
    expect(cert.checkPrivateKey(createPrivateKey(identity.keyPem))).toBe(true);
    expect(cert.ca).toBe(false);
    expect(cert.keyUsage).toEqual(["1.3.6.1.5.5.7.3.1"]);
    expect(cert.raw.includes(Buffer.from("0603551d130101ff04023000", "hex"))).toBe(true);
    expect(cert.raw.includes(Buffer.from("0603551d0f0101ff0404030205a0", "hex"))).toBe(true);
    expect(
      cert.raw.includes(
        Buffer.from(`0603551d23041830168014${ca.keyIdentifier.toString("hex")}`, "hex"),
      ),
    ).toBe(true);
    const expectedExpiry = new Date(cert.validFrom);
    expectedExpiry.setFullYear(expectedExpiry.getFullYear() + 1);
    expect(new Date(cert.validTo)).toEqual(expectedExpiry);
    if (hostname === "server.example.com") {
      expect(cert.checkHost(hostname)).toBe(hostname);
      expect(cert.checkIP("192.0.2.1")).toBeUndefined();
    } else {
      expect(cert.checkIP(hostname)).toBe(hostname);
      expect(cert.checkIP("192.0.2.2")).toBeUndefined();
    }
  });

  test("preserves SDK timestamp serials and fresh keys and rejects an unrelated signer or a corrupt signature", () => {
    const ca = generateCA();
    const otherCa = generateCA();
    const now = vi
      .spyOn(Date, "now")
      .mockReturnValueOnce(1767225600000)
      .mockReturnValueOnce(1767225600001);
    try {
      const first = createIdentityForHost("server.example.com", ca);
      const second = createIdentityForHost("server.example.com", ca);
      const cert = new X509Certificate(first.certPem);
      expect(first.keyPem).not.toBe(second.keyPem);
      expect(cert.serialNumber).toBe("01767225600000");
      expect(new X509Certificate(second.certPem).serialNumber).toBe("01767225600001");
      expect(cert.verify(otherCa.caCert.publicKey)).toBe(false);
      const corrupt = Buffer.from(cert.raw);
      corrupt[corrupt.length - 1] ^= 1;
      expect(new X509Certificate(corrupt).verify(ca.caCert.publicKey)).toBe(false);
    } finally {
      now.mockRestore();
    }
  });

  test("encodes UTF-8 names, long DER lengths, positive integers and GeneralizedTime", () => {
    const ca = createTestCertificate({
      commonName: `Certificate ${"\u00e9".repeat(150)}`,
      serialNumber: "80",
      notBefore: new Date("2049-12-31T00:00:00Z"),
      notAfter: new Date("2050-01-02T00:00:00Z"),
      ca: true,
    });
    expect(ca.caCert.subject).toContain("\u00e9".repeat(150));
    expect(ca.caCert.serialNumber).toBe("80");
    expect(ca.caCert.verify(ca.caCert.publicKey)).toBe(true);
    expect(new Date(ca.caCert.validFrom).toISOString()).toBe("2049-12-31T00:00:00.000Z");
    expect(new Date(ca.caCert.validTo).toISOString()).toBe("2050-01-02T00:00:00.000Z");
  });

  test("encodes a constrained intermediate and its authority information access URL", () => {
    const root = generateCA();
    const validity = {
      notBefore: new Date("2026-01-01T00:00:00Z"),
      notAfter: new Date("2027-01-01T00:00:00Z"),
    };
    const intermediate = createTestCertificate({
      commonName: "Intermediate",
      issuer: root,
      ca: true,
      pathLength: 0,
      ...validity,
    });
    const leaf = createTestCertificate({
      commonName: "localhost",
      issuer: intermediate,
      purpose: "server",
      hosts: ["localhost", "127.0.0.1", "::1"],
      issuerUrl: "http://127.0.0.1:1234/intermediate.crt",
      ...validity,
    });
    expect(intermediate.caCert.verify(root.caCert.publicKey)).toBe(true);
    expect(intermediate.caCert.ca).toBe(true);
    expect(
      intermediate.caCert.raw.includes(Buffer.from("0603551d130101ff040830060101ff020100", "hex")),
    ).toBe(true);
    expect(leaf.caCert.verify(intermediate.caCert.publicKey)).toBe(true);
    expect(leaf.caCert.checkIssued(intermediate.caCert)).toBe(true);
    expect(leaf.caCert.infoAccess).toBe("CA Issuers - URI:http://127.0.0.1:1234/intermediate.crt");
    expect(leaf.caCert.checkHost("localhost")).toBe("localhost");
    expect(leaf.caCert.checkIP("127.0.0.1")).toBe("127.0.0.1");
    expect(leaf.caCert.checkIP("::1")).toBe("::1");
  });

  test.each(["", "GG", "-1"])(
    "rejects an invalid serial %j rather than silently truncating it",
    (serialNumber) => {
      expect(() =>
        createTestCertificate({
          commonName: "Invalid serial",
          keys: generateCertificateKeys(),
          serialNumber,
          notBefore: new Date(),
          notAfter: new Date(),
        }),
      ).toThrow("Invalid certificate integer");
    },
  );
});
