#!/usr/bin/env node

/**
 * Deploy Source Escrow Script
 * 
 * This script creates a source escrow for cross-chain atomic swaps.
 * Source escrows are used when swapping FROM Cardano TO another chain.
 * 
 * Usage: node deploy-escrow-src.js [options]
 */

const CardanoSwapUtils = require('./utils');
const crypto = require('crypto');

async function deploySourceEscrow() {
    console.log('🚀 DEPLOYING SOURCE ESCROW');
    console.log('==========================');
    console.log('');
    
    const utils = new CardanoSwapUtils();
    
    // Generate demo participants
    const maker = utils.getWalletAddress();  // User wanting to swap from Cardano
    const taker = utils.generateAddress('demo_taker');  // User receiving on destination chain
    const secret = utils.generateSecret();
    
    console.log('👥 Swap Participants:');
    console.log(`   Maker (Cardano): ${maker}`);
    console.log(`   Taker (Destination): ${taker}`);
    console.log('');
    
    // Create swap parameters
    const swapParams = {
        orderHash: utils.generateOrderHash(maker, taker, utils.defaultAmounts.swapAmount),
        secret: secret,
        secretHash: utils.hashSecret(secret),
        maker: maker,
        taker: taker,
        amount: utils.defaultAmounts.swapAmount,
        safetyDeposit: utils.defaultAmounts.safetyDeposit,
        timelocks: utils.createTimelocks()
    };
    
    console.log('💰 Swap Details:');
    console.log(`   Swap Amount: ${swapParams.amount / 1000000} ADA`);
    console.log(`   Safety Deposit: ${swapParams.safetyDeposit / 1000000} ADA`);
    console.log(`   Total Required: ${(swapParams.amount + swapParams.safetyDeposit) / 1000000} ADA`);
    console.log(`   Order Hash: ${swapParams.orderHash}`);
    console.log('');
    
    console.log('⏰ Timelock Schedule:');
    const locks = swapParams.timelocks;
    console.log(`   Deploy Time: ${new Date(locks.deployTime * 1000).toLocaleString()}`);
    console.log(`   Src Withdraw: ${new Date(locks.srcWithdrawTime * 1000).toLocaleString()}`);
    console.log(`   Src Public Withdraw: ${new Date(locks.srcPublicWithdrawTime * 1000).toLocaleString()}`);
    console.log(`   Src Cancel: ${new Date(locks.srcCancelTime * 1000).toLocaleString()}`);
    console.log(`   Src Public Cancel: ${new Date(locks.srcPublicCancelTime * 1000).toLocaleString()}`);
    console.log('');
    
    // Create the source escrow through factory
    console.log('🏭 Creating Source Escrow via Factory...');
    
    const factoryParams = {
        action: 'CreateSrcEscrow',
        orderHash: swapParams.orderHash,
        secret: secret,
        maker: maker,
        taker: taker,
        amount: swapParams.amount,
        safetyDeposit: swapParams.safetyDeposit,
        timelocks: swapParams.timelocks,
        caller: maker
    };
    
    try {
        const result = await utils.simulateFactoryAction('CreateSrcEscrow', factoryParams);
        
        if (result.success) {
            console.log('✅ Source Escrow deployed successfully!');
            console.log('');
            
            // Save deployment info for other scripts
            const deploymentInfo = {
                type: 'SourceEscrow',
                escrowAddress: result.escrowAddress,
                orderHash: swapParams.orderHash,
                secret: secret,  // In real deployment, keep this secure!
                secretHash: swapParams.secretHash,
                maker: maker,
                taker: taker,
                amount: swapParams.amount,
                safetyDeposit: swapParams.safetyDeposit,
                timelocks: swapParams.timelocks,
                txHash: result.txHash,
                deployedAt: result.timestamp,
                network: utils.network
            };
            
            require('fs').writeFileSync(
                'src-escrow-deployment.json', 
                JSON.stringify(deploymentInfo, null, 2)
            );
            
            utils.displayTxSummary('Deploy Source Escrow', result, {
                'Escrow Type': 'Source (Cardano → Destination)',
                'Swap Amount': `${swapParams.amount / 1000000} ADA`,
                'Safety Deposit': `${swapParams.safetyDeposit / 1000000} ADA`,
                'Maker': maker,
                'Taker': taker
            });
            
            console.log('💾 Deployment saved to: src-escrow-deployment.json');
            console.log('');
            console.log('🔄 NEXT STEPS:');
            console.log('1. Share the order hash and secret hash with the taker');
            console.log('2. Taker creates corresponding destination escrow');
            console.log('3. Maker can withdraw using: node withdraw-src.js');
            console.log('4. Or cancel using: node cancel-src.js');
            console.log('');
            console.log('⚠️  SECRET (keep secure):');
            console.log(`   ${secret}`);
            
        } else {
            console.error('❌ Failed to deploy source escrow:', result.error);
        }
        
    } catch (error) {
        console.error('❌ Deployment failed:', error.message);
    }
}

if (require.main === module) {
    deploySourceEscrow().catch(console.error);
}

module.exports = { deploySourceEscrow };
