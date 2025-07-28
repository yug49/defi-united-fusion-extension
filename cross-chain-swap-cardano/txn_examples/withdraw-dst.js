#!/usr/bin/env node

/**
 * Withdraw from Destination Escrow Script
 * 
 * This script withdraws funds from a destination escrow using the secret
 * revealed on the source chain. This completes the cross-chain atomic swap.
 * 
 * Usage: node withdraw-dst.js [secret]
 */

const CardanoSwapUtils = require('./utils');

async function withdrawFromDestination(providedSecret = null) {
    console.log('💸 WITHDRAWING FROM DESTINATION ESCROW');
    console.log('======================================');
    console.log('');
    
    const utils = new CardanoSwapUtils();
    
    // Load destination escrow deployment info
    let escrowInfo;
    try {
        escrowInfo = JSON.parse(require('fs').readFileSync('dst-escrow-deployment.json', 'utf8'));
        console.log('📖 Loaded destination escrow deployment info');
    } catch (error) {
        console.error('❌ Could not load dst-escrow-deployment.json');
        console.log('   Run deploy-escrow-dst.js first');
        return;
    }
    
    // Try to get secret from source escrow withdrawal if not provided
    let secret = providedSecret;
    if (!secret) {
        try {
            const srcInfo = JSON.parse(require('fs').readFileSync('src-escrow-deployment.json', 'utf8'));
            if (srcInfo.status === 'withdrawn') {
                secret = srcInfo.secret;
                console.log('🔍 Using secret revealed from source escrow withdrawal');
            } else {
                console.log('⚠️  Source escrow not yet withdrawn, secret not revealed');
            }
        } catch (error) {
            console.log('⚠️  No source escrow info found');
        }
    }
    
    if (!secret) {
        console.error('❌ No secret provided and source escrow secret not available');
        console.log('   Usage: node withdraw-dst.js <secret>');
        console.log('   Or ensure source escrow has been withdrawn first');
        return;
    }
    
    const caller = utils.getWalletAddress();
    
    console.log('📋 Withdrawal Details:');
    console.log(`   Escrow Address: ${escrowInfo.escrowAddress}`);
    console.log(`   Withdrawal Amount: ${escrowInfo.amount / 1000000} ADA`);
    console.log(`   Safety Deposit: ${escrowInfo.safetyDeposit / 1000000} ADA`);
    console.log(`   Caller: ${caller}`);
    console.log('');
    
    // Check if withdrawal is allowed based on timelock
    const currentTime = Math.floor(Date.now() / 1000);
    const timelocks = escrowInfo.timelocks;
    
    console.log('⏰ Timelock Status:');
    console.log(`   Current Time: ${new Date(currentTime * 1000).toLocaleString()}`);
    console.log(`   Dst Withdraw Window: ${new Date(timelocks.dstWithdrawTime * 1000).toLocaleString()} - ${new Date(timelocks.dstPublicWithdrawTime * 1000).toLocaleString()}`);
    console.log(`   Dst Public Withdraw: ${new Date(timelocks.dstPublicWithdrawTime * 1000).toLocaleString()}`);
    
    // Determine which withdrawal method to use
    let withdrawalAction;
    let isAuthorized = false;
    
    if (utils.checkTimeWindow(timelocks, 'DstWithdraw', currentTime)) {
        withdrawalAction = 'DstWithdraw';
        isAuthorized = (caller === escrowInfo.maker);
        console.log(`   ✅ Private withdrawal window (maker only)`);
    } else if (utils.checkTimeWindow(timelocks, 'DstPublicWithdraw', currentTime)) {
        withdrawalAction = 'DstPublicWithdraw';
        isAuthorized = true;  // Anyone can withdraw in public window
        console.log(`   ✅ Public withdrawal window (anyone can withdraw)`);
    } else {
        console.log(`   ❌ Withdrawal not allowed at this time`);
        console.log('');
        console.log('⚠️  WITHDRAWAL WINDOWS:');
        console.log(`   Wait until: ${new Date(timelocks.dstWithdrawTime * 1000).toLocaleString()}`);
        return;
    }
    
    if (!isAuthorized && withdrawalAction === 'DstWithdraw') {
        console.log(`   ❌ Only maker (${escrowInfo.maker}) can withdraw during private window`);
        return;
    }
    
    console.log('');
    
    // Verify secret
    const providedSecretHash = utils.hashSecret(secret);
    if (providedSecretHash !== escrowInfo.secretHash) {
        console.error('❌ Invalid secret provided!');
        console.log(`   Expected hash: ${escrowInfo.secretHash}`);
        console.log(`   Provided hash: ${providedSecretHash}`);
        return;
    }
    
    console.log('✅ Secret verified successfully');
    console.log('');
    
    // Prepare withdrawal parameters
    const withdrawalParams = {
        action: withdrawalAction,
        secret: secret,
        caller: caller,
        escrowAddress: escrowInfo.escrowAddress,
        orderHash: escrowInfo.orderHash
    };
    
    // Add resolver for public withdrawal if needed
    if (withdrawalAction === 'DstPublicWithdraw') {
        withdrawalParams.resolver = utils.lopAddress;  // Use LOP contract as resolver
    }
    
    // Execute withdrawal
    console.log(`🔄 Executing ${withdrawalAction}...`);
    
    try {
        const result = await utils.simulateFactoryAction(withdrawalAction, withdrawalParams);
        
        if (result.success) {
            console.log('✅ Withdrawal successful!');
            console.log('');
            
            // Update deployment info
            escrowInfo.withdrawnAt = result.timestamp;
            escrowInfo.withdrawalTxHash = result.txHash;
            escrowInfo.withdrawnBy = caller;
            escrowInfo.status = 'withdrawn';
            
            require('fs').writeFileSync(
                'dst-escrow-deployment.json', 
                JSON.stringify(escrowInfo, null, 2)
            );
            
            utils.displayTxSummary('Destination Withdrawal', result, {
                'Action Type': withdrawalAction,
                'Amount': `${escrowInfo.amount / 1000000} ADA`,
                'Safety Deposit': `${escrowInfo.safetyDeposit / 1000000} ADA`,
                'Withdrawn By': caller,
                'Secret Used': secret.substring(0, 16) + '...'
            });
            
            console.log('🎉 CROSS-CHAIN SWAP COMPLETED!');
            console.log('');
            console.log('✅ Both escrows have been withdrawn using the same secret');
            console.log('✅ Atomic swap successfully executed across chains');
            console.log('✅ Funds have been safely transferred');
            console.log('');
            console.log('📊 SWAP SUMMARY:');
            console.log(`   Source Chain → Cardano: ${escrowInfo.amount / 1000000} ADA`);
            console.log(`   Secret: ${secret}`);
            console.log(`   Completed: ${new Date().toLocaleString()}`);
            
        } else {
            console.error('❌ Withdrawal failed:', result.error);
        }
        
    } catch (error) {
        console.error('❌ Withdrawal failed:', error.message);
    }
}

if (require.main === module) {
    const secret = process.argv[2];
    withdrawFromDestination(secret).catch(console.error);
}

module.exports = { withdrawFromDestination };
