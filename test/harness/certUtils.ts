/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

import tls from "tls";
import { createTestCertificate, type TestCertificate } from "./testCertificate.js";

export type CaData = TestCertificate;

function oneYearValidity(): { notBefore: Date; notAfter: Date } {
  const notBefore = new Date();
  const notAfter = new Date(notBefore);
  notAfter.setFullYear(notAfter.getFullYear() + 1);
  return { notBefore, notAfter };
}

export function generateCA(): CaData {
  return createTestCertificate({
    commonName: "SDK E2E Test CA",
    organization: "Copilot SDK Tests",
    serialNumber: "01",
    ca: true,
    ...oneYearValidity(),
  });
}

export interface IdentityData {
  certPem: string;
  keyPem: string;
}

export function createSecureContextForHost(hostname: string, ca: CaData): tls.SecureContext {
  const identity = createIdentityForHost(hostname, ca);
  return tls.createSecureContext({
    key: identity.keyPem,
    cert: identity.certPem,
    ca: ca.certPem,
  });
}

/**
 * IP literals get IP SANs; serverAuth, key usage and basic constraints keep
 * strict platform verifiers (macOS SecTrust) from rejecting the chain.
 */
export function createIdentityForHost(hostname: string, ca: CaData): IdentityData {
  return createTestCertificate({
    commonName: hostname,
    issuer: ca,
    serialNumber: String(Date.now()),
    purpose: "server",
    hosts: [hostname],
    ...oneYearValidity(),
  });
}
