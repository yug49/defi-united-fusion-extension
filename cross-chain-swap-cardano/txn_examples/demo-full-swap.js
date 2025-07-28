#!/usr/bin/env node

/**
 * Cross-Chain Swap Demo Script
 * 
 * This script demonstrates the complete cross-chain atomic swap flow:
 * 1. Deploy source escrow (Cardano → Other chain)
 * 2. Deploy destination escrow (Other chain → Cardano)
 * 3. Execute withdrawal on source (reveals secret)
 * 4. Execute withdrawal on destination (completes swap)
 * 
 * Usage: node demo-full-swap.js [mode]
 * Modes: full, deploy-only, withdraw-only, cancel-demo
 */

const CardanoSwapUtils = require('./utils');
const { deploySourceEscrow } = require('./deploy-escrow-src');
const { deployDestinationEscrow } = require('./deploy-escrow-dst');
const { withdrawFromSource } = require('./withdraw-src');
const { withdrawFromDestination } = require('./withdraw-dst');
const { cancelSourceEscrow } = require('./cancel-src');
const { cancelDestinationEscrow } = require('./cancel-dst');

async function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function fullSwapDemo() {
    console.log('🌐 CROSS-CHAIN ATOMIC SWAP DEMO');
    console.log('===============================');
    console.log('');
    console.log('This demo shows a complete atomic swap between Cardano and another chain');
    console.log('using hash time lock contracts (HTLCs) for trustless execution.');
    console.log('');
    
    const utils = new CardanoSwapUtils();
    
    console.log('📋 DEMO SCENARIO:');
    console.log('   Alice wants to send 5 ADA from Cardano to Bob');
    console.log('   Bob wants to send equivalent tokens from Ethereum to Alice');
    console.log('   They use atomic swaps to execute this trustlessly');
    console.log('');
    
    try {
        console.log('🎬 STEP 1: Deploy Source Escrow (Alice locks ADA on Cardano)');
        console.log('============================================================');
        await deploySourceEscrow();
        
        console.log('\n⏳ Waiting 2 seconds before next step...\n');
        await sleep(2000);
        
        console.log('🎬 STEP 2: Deploy Destination Escrow (Bob locks tokens on destination)');
        console.log('=====================================================================');
        await deployDestinationEscrow();
        
        console.log('\n⏳ Waiting 2 seconds before next step...\n');
        await sleep(2000);
        
        console.log('🎬 STEP 3: Alice withdraws from destination (reveals secret)');
        console.log('===========================================================');
        await withdrawFromSource();
        
        console.log('\n⏳ Waiting 2 seconds before next step...\n');
        await sleep(2000);
        
        console.log('🎬 STEP 4: Bob withdraws from source using revealed secret');
        console.log('==========================================================');
        await withdrawFromDestination();
        
        console.log('\n🎉 ATOMIC SWAP COMPLETED SUCCESSFULLY!');
        console.log('======================================');
        console.log('');
        console.log('✅ Both parties have received their desired tokens');
        console.log('✅ No trust was required between the parties');
        console.log('✅ The swap was atomic - either both succeed or both fail');
        console.log('✅ Secret was revealed only after first withdrawal');
        console.log('');
        console.log('📊 FINAL SUMMARY:');
        
        // Display final state
        try {
            const srcInfo = JSON.parse(require('fs').readFileSync('src-escrow-deployment.json', 'utf8'));
            const dstInfo = JSON.parse(require('fs').readFileSync('dst-escrow-deployment.json', 'utf8'));
            
            console.log(`   Source Escrow: ${srcInfo.status.toUpperCase()}`);
            console.log(`   Destination Escrow: ${dstInfo.status.toUpperCase()}`);
            console.log(`   Amount Swapped: ${srcInfo.amount / 1000000} ADA`);
            console.log(`   Secret Used: ${srcInfo.secret}`);
            console.log(`   Completed: ${new Date().toLocaleString()}`);
        } catch (error) {
            console.log('   Could not load final state details');
        }
        
    } catch (error) {
        console.error('❌ Demo failed:', error.message);
    }
}

async function deployOnlyDemo() {
    console.log('🏗️  DEPLOYMENT ONLY DEMO');
    console.log('========================');
    console.log('');
    
    console.log('Step 1: Deploying Source Escrow...');
    await deploySourceEscrow();
    
    console.log('\nStep 2: Deploying Destination Escrow...');
    await deployDestinationEscrow();
    
    console.log('\n✅ Both escrows deployed! Use withdraw scripts to complete the swap.');
}

async function withdrawOnlyDemo() {
    console.log('💸 WITHDRAWAL ONLY DEMO');
    console.log('=======================');
    console.log('');
    
    console.log('Step 1: Withdrawing from Source Escrow...');
    await withdrawFromSource();
    
    console.log('\nStep 2: Withdrawing from Destination Escrow...');
    await withdrawFromDestination();
    
    console.log('\n✅ Withdrawals completed!');
}

async function cancellationDemo() {
    console.log('🚫 CANCELLATION DEMO');
    console.log('====================');
    console.log('');
    console.log('This demo shows how to cancel escrows if the swap needs to be aborted');
    console.log('');
    
    // First deploy escrows
    console.log('Step 1: Deploy escrows for cancellation demo...');
    await deploySourceEscrow();
    await deployDestinationEscrow();
    
    console.log('\nStep 2: Cancel Source Escrow...');
    await cancelSourceEscrow();
    
    console.log('\nStep 3: Cancel Destination Escrow...');
    await cancelDestinationEscrow();
    
    console.log('\n✅ Both escrows cancelled! Funds have been refunded.');
}

async function interactiveDemo() {
    console.log('🎮 INTERACTIVE CROSS-CHAIN SWAP DEMO');
    console.log('====================================');
    console.log('');
    console.log('Select a demo mode:');
    console.log('1. Full swap demo (complete atomic swap)');
    console.log('2. Deploy only (create escrows)');
    console.log('3. Withdraw only (complete existing escrows)');
    console.log('4. Cancellation demo (abort swap)');
    console.log('');
    
    const readline = require('readline');
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    
    rl.question('Enter your choice (1-4): ', async (choice) => {
        rl.close();
        
        switch (choice) {
            case '1':
                await fullSwapDemo();
                break;
            case '2':
                await deployOnlyDemo();
                break;
            case '3':
                await withdrawOnlyDemo();
                break;
            case '4':
                await cancellationDemo();
                break;
            default:
                console.log('Invalid choice. Running full demo...');
                await fullSwapDemo();
        }
    });
}

async function main() {
    const mode = process.argv[2];
    
    switch (mode) {
        case 'full':
            await fullSwapDemo();
            break;
        case 'deploy-only':
            await deployOnlyDemo();
            break;
        case 'withdraw-only':
            await withdrawOnlyDemo();
            break;
        case 'cancel-demo':
            await cancellationDemo();
            break;
        case 'interactive':
            await interactiveDemo();
            break;
        default:
            console.log('🎮 Cross-Chain Swap Demo');
            console.log('========================');
            console.log('');
            console.log('Usage: node demo-full-swap.js [mode]');
            console.log('');
            console.log('Available modes:');
            console.log('  full          - Complete atomic swap demo');
            console.log('  deploy-only   - Deploy escrows only');
            console.log('  withdraw-only - Withdraw from existing escrows');
            console.log('  cancel-demo   - Cancellation demo');
            console.log('  interactive   - Interactive mode with menu');
            console.log('');
            console.log('Running interactive mode...');
            await interactiveDemo();
    }
}

if (require.main === module) {
    main().catch(console.error);
}

module.exports = {
    fullSwapDemo,
    deployOnlyDemo,
    withdrawOnlyDemo,
    cancellationDemo
};
