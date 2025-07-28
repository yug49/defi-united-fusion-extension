#!/usr/bin/env node

/**
 * Withdraw from Source Escrow Script
 * 
 * This script withdraws funds from a source escrow after revealing the secret.
 * This is typically done by the taker after they've confirmed the destination
 * escrow is properly funded.
 * 
 * Usage: node withdraw-src.js [secret] [target]
 */

const CardanoSwapUtils = require('./utils');

async function withdrawFromSource(providedSecret = null, targetAddress = null) {
    console.log('💸 WITHDRAWING FROM SOURCE ESCROW');
    console.log('=================================');
    console.log('');
    
    const utils = new CardanoSwapUtils();
    
    // Load source escrow deployment info
    let escrowInfo;
    try {
        escrowInfo = JSON.parse(require('fs').readFileSync('src-escrow-deployment.json', 'utf8'));
        console.log('📖 Loaded source escrow deployment info');
    } catch (error) {
        console.error('❌ Could not load src-escrow-deployment.json');
        console.log('   Run deploy-escrow-src.js first');
        return;
    }
    
    const secret = providedSecret || escrowInfo.secret;
    const target = targetAddress || escrowInfo.taker;
    const caller = utils.getWalletAddress();
    
    console.log('📋 Withdrawal Details:');
    console.log(`   Escrow Address: ${escrowInfo.escrowAddress}`);
    console.log(`   Withdrawal Amount: ${escrowInfo.amount / 1000000} ADA`);
    console.log(`   Safety Deposit: ${escrowInfo.safetyDeposit / 1000000} ADA`);
    console.log(`   Target Address: ${target}`);
    console.log(`   Caller: ${caller}`);
    console.log('');
    
    // Check if withdrawal is allowed based on timelock
    const currentTime = Math.floor(Date.now() / 1000);
    const timelocks = escrowInfo.timelocks;
    
    console.log('⏰ Timelock Status:');
    console.log(`   Current Time: ${new Date(currentTime * 1000).toLocaleString()}`);
    console.log(`   Src Withdraw Window: ${new Date(timelocks.srcWithdrawTime * 1000).toLocaleString()} - ${new Date(timelocks.srcPublicWithdrawTime * 1000).toLocaleString()}`);
    console.log(`   Src Public Withdraw: ${new Date(timelocks.srcPublicWithdrawTime * 1000).toLocaleString()}`);
    
    // Determine which withdrawal method to use
    let withdrawalAction;
    let isAuthorized = false;
    
    if (utils.checkTimeWindow(timelocks, 'SrcWithdraw', currentTime)) {
        withdrawalAction = 'SrcWithdraw';
        isAuthorized = (caller === escrowInfo.taker);
        console.log(`   ✅ Private withdrawal window (taker only)`);
    } else if (utils.checkTimeWindow(timelocks, 'SrcPublicWithdraw', currentTime)) {
        withdrawalAction = 'SrcPublicWithdraw';
        isAuthorized = true;  // Anyone can withdraw in public window
        console.log(`   ✅ Public withdrawal window (anyone can withdraw)`);
    } else {
        console.log(`   ❌ Withdrawal not allowed at this time`);
        console.log('');
        console.log('⚠️  WITHDRAWAL WINDOWS:');
        console.log(`   Wait until: ${new Date(timelocks.srcWithdrawTime * 1000).toLocaleString()}`);
        return;
    }
    
    if (!isAuthorized && withdrawalAction === 'SrcWithdraw') {
        console.log(`   ❌ Only taker (${escrowInfo.taker}) can withdraw during private window`);
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
        target: target,
        escrowAddress: escrowInfo.escrowAddress,
        orderHash: escrowInfo.orderHash
    };
    
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
            escrowInfo.withdrawnTo = target;
            escrowInfo.status = 'withdrawn';
            
            require('fs').writeFileSync(
                'src-escrow-deployment.json', 
                JSON.stringify(escrowInfo, null, 2)
            );
            
            utils.displayTxSummary('Source Withdrawal', result, {
                'Action Type': withdrawalAction,
                'Amount': `${escrowInfo.amount / 1000000} ADA`,
                'Safety Deposit': `${escrowInfo.safetyDeposit / 1000000} ADA`,
                'Withdrawn By': caller,
                'Withdrawn To': target
            });
            
            console.log('🔓 SECRET REVEALED:');
            console.log(`   ${secret}`);
            console.log('');
            console.log('⚠️  IMPORTANT: Secret is now public on the blockchain!');
            console.log('   Anyone can use this secret to withdraw from the destination escrow.');
            console.log('');
            console.log('🔄 NEXT STEPS:');
            console.log('1. Secret is now revealed and can be used on destination chain');
            console.log('2. Destination escrow can be withdrawn using this secret');
            console.log('3. Cross-chain atomic swap is now complete!');
            
        } else {
            console.error('❌ Withdrawal failed:', result.error);
        }
        
    } catch (error) {
        console.error('❌ Withdrawal failed:', error.message);
    }
}

if (require.main === module) {
    const secret = process.argv[2];
    const target = process.argv[3];
    withdrawFromSource(secret, target).catch(console.error);
}

module.exports = { withdrawFromSource };
