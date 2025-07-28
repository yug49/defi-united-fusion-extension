#!/usr/bin/env node

/**
 * Deploy Destination Escrow Script
 * 
 * This script creates a destination escrow for cross-chain atomic swaps.
 * Destination escrows are used when swapping TO Cardano FROM another chain.
 * 
 * Usage: node deploy-escrow-dst.js [orderHash] [srcCancellationTime]
 */

const CardanoSwapUtils = require('./utils');
const crypto = require('crypto');

async function deployDestinationEscrow(providedOrderHash = null, srcCancellationTime = null) {
    console.log('🚀 DEPLOYING DESTINATION ESCROW');
    console.log('===============================');
    console.log('');
    
    const utils = new CardanoSwapUtils();
    
    // Try to load from source escrow deployment or use provided parameters
    let srcEscrowInfo = null;
    if (!providedOrderHash) {
        try {
            srcEscrowInfo = JSON.parse(require('fs').readFileSync('src-escrow-deployment.json', 'utf8'));
            console.log('📖 Loaded source escrow info from src-escrow-deployment.json');
        } catch (error) {
            console.log('⚠️  No source escrow deployment found, creating standalone destination escrow');
        }
    }
    
    // Generate demo participants (in real scenario, taker is from source chain)
    const maker = srcEscrowInfo ? srcEscrowInfo.taker : utils.generateAddress('demo_maker');  // From source chain
    const taker = utils.getWalletAddress();  // User receiving on Cardano
    const orderHash = providedOrderHash || (srcEscrowInfo ? srcEscrowInfo.orderHash : utils.generateOrderHash(maker, taker, utils.defaultAmounts.swapAmount));
    const secret = srcEscrowInfo ? srcEscrowInfo.secret : utils.generateSecret();
    
    console.log('👥 Swap Participants:');
    console.log(`   Maker (Source Chain): ${maker}`);
    console.log(`   Taker (Cardano): ${taker}`);
    console.log('');
    
    // Create destination-specific timelock schedule
    const customTimelocks = srcEscrowInfo ? {
        // Align with source escrow timing
        dstWithdrawTime: 200,    // Earlier than source withdraw
        dstPublicWithdrawTime: 500,
        dstCancelTime: 800       // Before source cancel
    } : {};
    
    const timelocks = utils.createTimelocks(null, customTimelocks);
    const srcCancelTime = srcCancellationTime || (srcEscrowInfo ? srcEscrowInfo.timelocks.srcCancelTime : timelocks.deployTime + 1200);
    
    // Create swap parameters
    const swapParams = {
        orderHash: orderHash,
        secret: secret,
        secretHash: utils.hashSecret(secret),
        maker: maker,
        taker: taker,
        amount: srcEscrowInfo ? srcEscrowInfo.amount : utils.defaultAmounts.swapAmount,
        safetyDeposit: srcEscrowInfo ? srcEscrowInfo.safetyDeposit : utils.defaultAmounts.safetyDeposit,
        timelocks: timelocks,
        srcCancellationTime: srcCancelTime
    };
    
    console.log('💰 Swap Details:');
    console.log(`   Swap Amount: ${swapParams.amount / 1000000} ADA`);
    console.log(`   Safety Deposit: ${swapParams.safetyDeposit / 1000000} ADA`);
    console.log(`   Total Required: ${(swapParams.amount + swapParams.safetyDeposit) / 1000000} ADA`);
    console.log(`   Order Hash: ${swapParams.orderHash}`);
    console.log(`   Source Cancel Time: ${new Date(srcCancelTime * 1000).toLocaleString()}`);
    console.log('');
    
    console.log('⏰ Destination Timelock Schedule:');
    const locks = swapParams.timelocks;
    console.log(`   Deploy Time: ${new Date(locks.deployTime * 1000).toLocaleString()}`);
    console.log(`   Dst Withdraw: ${new Date(locks.dstWithdrawTime * 1000).toLocaleString()}`);
    console.log(`   Dst Public Withdraw: ${new Date(locks.dstPublicWithdrawTime * 1000).toLocaleString()}`);
    console.log(`   Dst Cancel: ${new Date(locks.dstCancelTime * 1000).toLocaleString()}`);
    console.log('');
    
    // Create immutables for destination escrow
    const immutables = utils.createImmutables({
        orderHash: swapParams.orderHash,
        secret: swapParams.secret,
        maker: swapParams.maker,
        taker: swapParams.taker,
        amount: swapParams.amount,
        safetyDeposit: swapParams.safetyDeposit,
        timelocks: swapParams.timelocks
    });
    
    // Create the destination escrow through factory
    console.log('🏭 Creating Destination Escrow via Factory...');
    
    const factoryParams = {
        action: 'CreateDstEscrow',
        dstImmutables: immutables,
        srcCancellationTimestamp: swapParams.srcCancellationTime,
        providedBalance: swapParams.amount + swapParams.safetyDeposit,
        caller: taker
    };
    
    try {
        const result = await utils.simulateFactoryAction('CreateDstEscrow', factoryParams);
        
        if (result.success) {
            console.log('✅ Destination Escrow deployed successfully!');
            console.log('');
            
            // Save deployment info for other scripts
            const deploymentInfo = {
                type: 'DestinationEscrow',
                escrowAddress: result.escrowAddress,
                orderHash: swapParams.orderHash,
                secret: secret,  // In real deployment, keep this secure!
                secretHash: swapParams.secretHash,
                maker: maker,
                taker: taker,
                amount: swapParams.amount,
                safetyDeposit: swapParams.safetyDeposit,
                timelocks: swapParams.timelocks,
                srcCancellationTime: swapParams.srcCancellationTime,
                immutables: immutables,
                txHash: result.txHash,
                deployedAt: result.timestamp,
                network: utils.network
            };
            
            require('fs').writeFileSync(
                'dst-escrow-deployment.json', 
                JSON.stringify(deploymentInfo, null, 2)
            );
            
            utils.displayTxSummary('Deploy Destination Escrow', result, {
                'Escrow Type': 'Destination (Source → Cardano)',
                'Swap Amount': `${swapParams.amount / 1000000} ADA`,
                'Safety Deposit': `${swapParams.safetyDeposit / 1000000} ADA`,
                'Maker': maker,
                'Taker': taker,
                'Linked Source': srcEscrowInfo ? 'Yes' : 'Standalone'
            });
            
            console.log('💾 Deployment saved to: dst-escrow-deployment.json');
            console.log('');
            console.log('🔄 NEXT STEPS:');
            console.log('1. Wait for maker to reveal secret on source chain');
            console.log('2. Use revealed secret to withdraw: node withdraw-dst.js');
            console.log('3. Or cancel if needed: node cancel-dst.js');
            
            if (srcEscrowInfo) {
                console.log('');
                console.log('🔗 CROSS-CHAIN LINK:');
                console.log(`   Source Escrow: ${srcEscrowInfo.escrowAddress}`);
                console.log(`   Destination Escrow: ${result.escrowAddress}`);
                console.log('   Both escrows use the same order hash and secret!');
            }
            
        } else {
            console.error('❌ Failed to deploy destination escrow:', result.error);
        }
        
    } catch (error) {
        console.error('❌ Deployment failed:', error.message);
    }
}

if (require.main === module) {
    const orderHash = process.argv[2];
    const srcCancelTime = process.argv[3] ? parseInt(process.argv[3]) : null;
    deployDestinationEscrow(orderHash, srcCancelTime).catch(console.error);
}

module.exports = { deployDestinationEscrow };
