/**
 * Example: Use Copilot to audit a Solidity smart contract.
 *
 * This script demonstrates the one-shot `auditContract()` helper that sends
 * your contract source to Copilot with a security-auditor system prompt and
 * returns a structured AuditReport.
 *
 * Usage:
 *   npx tsx examples/audit_contract.ts
 */

import { CopilotSolidityClient } from "../src/index.js";

// A deliberately vulnerable contract for demonstration purposes
const VULNERABLE_CONTRACT = `
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract VulnerableVault {
    mapping(address => uint256) public balances;

    event Deposit(address indexed user, uint256 amount);
    event Withdrawal(address indexed user, uint256 amount);

    function deposit() external payable {
        balances[msg.sender] += msg.value;
        emit Deposit(msg.sender, msg.value);
    }

    // Vulnerable: sends ETH before updating state (reentrancy)
    function withdraw(uint256 amount) external {
        require(balances[msg.sender] >= amount, "Insufficient balance");

        (bool success, ) = msg.sender.call{value: amount}("");
        require(success, "Transfer failed");

        balances[msg.sender] -= amount;
        emit Withdrawal(msg.sender, amount);
    }

    // Vulnerable: no access control
    function drain() external {
        (bool success, ) = msg.sender.call{value: address(this).balance}("");
        require(success, "Transfer failed");
    }

    function getBalance() external view returns (uint256) {
        return address(this).balance;
    }
}
`;

async function main(): Promise<void> {
    const client = new CopilotSolidityClient({
        solidityVersion: "0.8.24",
        includeGasHints: true,
    });

    try {
        await client.start();
        console.log("Copilot CLI started. Running audit...\n");

        const report = await client.auditContract(
            VULNERABLE_CONTRACT,
            "VulnerableVault",
        );

        // Print the audit report
        console.log(`Contract: ${report.contractName}`);
        console.log(`Findings: ${report.findings.length}`);
        console.log("---");

        for (const finding of report.findings) {
            console.log(`[${finding.severity.toUpperCase()}] ${finding.title}`);
            if (finding.swcId) {
                console.log(`  SWC: ${finding.swcId}`);
            }
            console.log(`  ${finding.description}`);
            if (finding.recommendation) {
                console.log(`  Fix: ${finding.recommendation}`);
            }
            console.log();
        }

        console.log("Summary:", report.summary);
    } catch (error) {
        console.error("Audit failed:", error);
        process.exitCode = 1;
    } finally {
        await client.stop();
    }
}

main();
