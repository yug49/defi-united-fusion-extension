#!/usr/bin/env node

/**
 * Cancel Destination Escrow Script
 * 
 * This script cancels a destination escrow and refunds the locked funds.
 * This is used when the cross-chain swap needs to be aborted on the destination side.
 * 
 * Usage: node cancel-dst.js [refundTarget]
 */

const CardanoSwapUtils = require('./utils');

async function cancelDestinationEscrow(refundTarget = null) {
    console.log('🚫 CANCELLING DESTINATION ESCROW');
    console.log('================================');
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
    const target = refundTarget || escrowInfo.taker;  // Refund to taker by default
    
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
    console.log(`   Dst Cancel Window: ${new Date(timelocks.dstCancelTime * 1000).toLocaleString()}`);
    console.log(`   Source Cancel Deadline: ${new Date(escrowInfo.srcCancellationTime * 1000).toLocaleString()}`);
    
    // Check if destination cancellation is allowed
    let cancellationAction;
    let isAuthorized = false;
    
    if (utils.checkTimeWindow(timelocks, 'DstCancel', currentTime)) {
        cancellationAction = 'DstCancel';
        isAuthorized = (caller === escrowInfo.taker);
        console.log(`   ✅ Destination cancellation window (taker only)`);
    } else if (currentTime >= timelocks.dstCancelTime) {
        cancellationAction = 'DstPublicCancel';
        isAuthorized = true;  // Anyone can cancel after deadline
        console.log(`   ✅ Public cancellation allowed (anyone can cancel)`);
    } else {
        console.log(`   ❌ Cancellation not allowed at this time`);
        console.log('');
        console.log('⚠️  CANCELLATION WINDOW:');
        console.log(`   Wait until: ${new Date(timelocks.dstCancelTime * 1000).toLocaleString()}`);
        return;
    }
    
    if (!isAuthorized && cancellationAction === 'DstCancel') {
        console.log(`   ❌ Only taker (${escrowInfo.taker}) can cancel during private window`);
        return;
    }
    
    // Check if source escrow cancellation deadline has passed
    if (currentTime >= escrowInfo.srcCancellationTime) {
        console.log(`   ⚠️  Source cancellation deadline has passed`);
        console.log(`   This suggests the source escrow may have been withdrawn`);
        console.log('');
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
    if (cancellationAction === 'DstPublicCancel') {
        cancellationParams.resolver = utils.lopAddress;  // Use LOP contract as resolver
    }
    
    console.log('⚠️  CANCELLATION WARNING:');
    console.log('   This will refund all locked funds and abort the swap on destination side');
    console.log('   Coordinate with source chain to ensure proper cancellation there too');
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
                'dst-escrow-deployment.json', 
                JSON.stringify(escrowInfo, null, 2)
            );
            
            utils.displayTxSummary('Destination Cancellation', result, {
                'Action Type': cancellationAction,
                'Refund Amount': `${(escrowInfo.amount + escrowInfo.safetyDeposit) / 1000000} ADA`,
                'Cancelled By': caller,
                'Refunded To': target
            });
            
            console.log('🔄 NEXT STEPS:');
            console.log('1. Destination escrow has been cancelled and funds refunded');
            console.log('2. Ensure source escrow is also cancelled if it exists');
            console.log('3. Cross-chain swap has been aborted on destination side');
            console.log('');
            console.log('📊 CANCELLATION SUMMARY:');
            console.log(`   Refunded: ${(escrowInfo.amount + escrowInfo.safetyDeposit) / 1000000} ADA`);
            console.log(`   To: ${target}`);
            console.log(`   Reason: ${cancellationAction === 'DstCancel' ? 'Private cancellation' : 'Public timeout'}`);
            console.log(`   Cancelled: ${new Date().toLocaleString()}`);
            
        } else {
            console.error('❌ Cancellation failed:', result.error);
        }
        
    } catch (error) {
        console.error('❌ Cancellation failed:', error.message);
    }
}

if (require.main === module) {
    const refundTarget = process.argv[2];
    cancelDestinationEscrow(refundTarget).catch(console.error);
}

module.exports = { cancelDestinationEscrow };
