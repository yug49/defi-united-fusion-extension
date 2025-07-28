#!/usr/bin/env node

/**
 * Cancel Source Escrow Script
 * 
 * This script cancels a source escrow and refunds the locked funds.
 * This is used when the cross-chain swap needs to be aborted.
 * 
 * Usage: node cancel-src.js [refundTarget]
 */

const CardanoSwapUtils = require('./utils');

async function cancelSourceEscrow(refundTarget = null) {
    console.log('🚫 CANCELLING SOURCE ESCROW');
    console.log('===========================');
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
    
    // Check if already withdrawn or cancelled
    if (escrowInfo.status === 'withdrawn') {
        console.log('❌ Escrow has already been withdrawn, cannot cancel');
        return;
    }
    
    if (escrowInfo.status === 'cancelled') {
        console.log('❌ Escrow has already been cancelled');
        return;
    }
    
    const caller = utils.getWalletAddress();
    const target = refundTarget || escrowInfo.maker;  // Refund to maker by default
    
    console.log('📋 Cancellation Details:');
    console.log(`   Escrow Address: ${escrowInfo.escrowAddress}`);
    console.log(`   Refund Amount: ${(escrowInfo.amount + escrowInfo.safetyDeposit) / 1000000} ADA`);
    console.log(`   Refund Target: ${target}`);
    console.log(`   Caller: ${caller}`);
    console.log('');
    
    // Check if cancellation is allowed based on timelock
    const currentTime = Math.floor(Date.now() / 1000);
    const timelocks = escrowInfo.timelocks;
    
    console.log('⏰ Timelock Status:');
    console.log(`   Current Time: ${new Date(currentTime * 1000).toLocaleString()}`);
    console.log(`   Src Cancel Window: ${new Date(timelocks.srcCancelTime * 1000).toLocaleString()} - ${new Date(timelocks.srcPublicCancelTime * 1000).toLocaleString()}`);
    console.log(`   Src Public Cancel: ${new Date(timelocks.srcPublicCancelTime * 1000).toLocaleString()}`);
    
    // Determine which cancellation method to use
    let cancellationAction;
    let isAuthorized = false;
    
    if (utils.checkTimeWindow(timelocks, 'SrcCancel', currentTime)) {
        cancellationAction = 'SrcCancel';
        isAuthorized = (caller === escrowInfo.taker);
        console.log(`   ✅ Private cancellation window (taker only)`);
    } else if (utils.checkTimeWindow(timelocks, 'SrcPublicCancel', currentTime)) {
        cancellationAction = 'SrcPublicCancel';
        isAuthorized = true;  // Anyone can cancel in public window
        console.log(`   ✅ Public cancellation window (anyone can cancel)`);
    } else {
        console.log(`   ❌ Cancellation not allowed at this time`);
        console.log('');
        console.log('⚠️  CANCELLATION WINDOWS:');
        console.log(`   Wait until: ${new Date(timelocks.srcCancelTime * 1000).toLocaleString()}`);
        return;
    }
    
    if (!isAuthorized && cancellationAction === 'SrcCancel') {
        console.log(`   ❌ Only taker (${escrowInfo.taker}) can cancel during private window`);
        return;
    }
    
    console.log('');
    
    // Prepare cancellation parameters
    const cancellationParams = {
        action: cancellationAction,
        caller: caller,
        refundTarget: target,
        escrowAddress: escrowInfo.escrowAddress,
        orderHash: escrowInfo.orderHash
    };
    
    // Add resolver for public cancellation if needed
    if (cancellationAction === 'SrcPublicCancel') {
        cancellationParams.resolver = utils.lopAddress;  // Use LOP contract as resolver
    }
    
    console.log('⚠️  CANCELLATION WARNING:');
    console.log('   This will refund all locked funds and abort the cross-chain swap');
    console.log('   Make sure the destination escrow is also cancelled if it exists');
    console.log('');
    
    // Execute cancellation
    console.log(`🔄 Executing ${cancellationAction}...`);
    
    try {
        const result = await utils.simulateFactoryAction(cancellationAction, cancellationParams);
        
        if (result.success) {
            console.log('✅ Cancellation successful!');
            console.log('');
            
            // Update deployment info
            escrowInfo.cancelledAt = result.timestamp;
            escrowInfo.cancellationTxHash = result.txHash;
            escrowInfo.cancelledBy = caller;
            escrowInfo.refundedTo = target;
            escrowInfo.status = 'cancelled';
            
            require('fs').writeFileSync(
                'src-escrow-deployment.json', 
                JSON.stringify(escrowInfo, null, 2)
            );
            
            utils.displayTxSummary('Source Cancellation', result, {
                'Action Type': cancellationAction,
                'Refund Amount': `${(escrowInfo.amount + escrowInfo.safetyDeposit) / 1000000} ADA`,
                'Cancelled By': caller,
                'Refunded To': target
            });
            
            console.log('🔄 NEXT STEPS:');
            console.log('1. Source escrow has been cancelled and funds refunded');
            console.log('2. If destination escrow exists, consider cancelling it too');
            console.log('3. Cross-chain swap has been aborted');
            console.log('');
            console.log('⚠️  IMPORTANT NOTES:');
            console.log('   - Secret has NOT been revealed');
            console.log('   - Destination escrow funds are still locked');
            console.log('   - Coordinate with other party to cancel destination escrow');
            
        } else {
            console.error('❌ Cancellation failed:', result.error);
        }
        
    } catch (error) {
        console.error('❌ Cancellation failed:', error.message);
    }
}

if (require.main === module) {
    const refundTarget = process.argv[2];
    cancelSourceEscrow(refundTarget).catch(console.error);
}

module.exports = { cancelSourceEscrow };
