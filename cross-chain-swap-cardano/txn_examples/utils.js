const crypto = require('crypto');
require('dotenv').config({ path: '../.env' });

/**
 * Utility functions for cross-chain swap interactions on Cardano
 */

class CardanoSwapUtils {
    constructor() {
        this.network = 'preprod';
        this.factoryAddress = 'addr_test1w9fbe9d3e7b9cf25f6b36d25ae170beb8a6ac8088a08ebd03311d';
        this.lopAddress = 'addr_test1wp203846488426adefdc379e46cc9713d1695f5dd424728694acb07f1e';
        
        // Standard timelock periods (in seconds)
        this.defaultTimelocks = {
            srcWithdrawTime: 300,        // 5 minutes
            srcPublicWithdrawTime: 600,  // 10 minutes  
            srcCancelTime: 900,          // 15 minutes
            srcPublicCancelTime: 1200,   // 20 minutes
            dstWithdrawTime: 450,        // 7.5 minutes
            dstPublicWithdrawTime: 750,  // 12.5 minutes
            dstCancelTime: 1050          // 17.5 minutes
        };
        
        // Standard amounts (in lovelace - 1 ADA = 1,000,000 lovelace)
        this.defaultAmounts = {
            swapAmount: 5000000,      // 5 ADA
            safetyDeposit: 2000000,   // 2 ADA
            minAmount: 1000000        // 1 ADA
        };
    }
    
    /**
     * Generate a cryptographically secure secret
     */
    generateSecret() {
        return crypto.randomBytes(32).toString('hex');
    }
    
    /**
     * Generate hash of secret (compatible with Cardano)
     */
    hashSecret(secret) {
        return crypto.createHash('sha256').update(secret, 'hex').digest('hex');
    }
    
    /**
     * Generate order hash from order parameters
     */
    generateOrderHash(maker, taker, amount, token = 'ADA') {
        const orderData = `${maker}${taker}${amount}${token}${Date.now()}`;
        return crypto.createHash('sha256').update(orderData).digest('hex');
    }
    
    /**
     * Generate Cardano address (simplified for demo)
     */
    generateAddress(seed = null) {
        const hash = crypto.createHash('sha256')
            .update(seed || crypto.randomBytes(32))
            .digest('hex');
        return `addr_test1q${hash.substring(0, 50)}`;
    }
    
    /**
     * Create timelock configuration
     */
    createTimelocks(deployTime = null, customTimelocks = {}) {
        const now = deployTime || Math.floor(Date.now() / 1000);
        const locks = { ...this.defaultTimelocks, ...customTimelocks };
        
        return {
            deployTime: now,
            srcWithdrawTime: now + locks.srcWithdrawTime,
            srcPublicWithdrawTime: now + locks.srcPublicWithdrawTime,
            srcCancelTime: now + locks.srcCancelTime,
            srcPublicCancelTime: now + locks.srcPublicCancelTime,
            dstWithdrawTime: now + locks.dstWithdrawTime,
            dstPublicWithdrawTime: now + locks.dstPublicWithdrawTime,
            dstCancelTime: now + locks.dstCancelTime
        };
    }
    
    /**
     * Create immutables structure for escrow
     */
    createImmutables(params) {
        const {
            orderHash,
            secret,
            maker,
            taker,
            amount,
            safetyDeposit,
            timelocks
        } = params;
        
        return {
            orderHash: orderHash || this.generateOrderHash(maker, taker, amount),
            secretHash: this.hashSecret(secret),
            makerAddr: maker,
            takerAddr: taker,
            escrowAmount: amount || this.defaultAmounts.swapAmount,
            safetyDeposit: safetyDeposit || this.defaultAmounts.safetyDeposit,
            timelocks: timelocks || this.createTimelocks()
        };
    }
    
    /**
     * Execute Haskell command through cabal
     */
    async executeHaskellCommand(command, args = []) {
        const { exec } = require('child_process');
        const util = require('util');
        const execAsync = util.promisify(exec);
        
        const fullCommand = `cabal run cross-chain-swap-cardano ${command} ${args.join(' ')}`;
        
        try {
            const result = await execAsync(fullCommand);
            return {
                success: true,
                stdout: result.stdout,
                stderr: result.stderr
            };
        } catch (error) {
            return {
                success: false,
                error: error.message,
                stdout: error.stdout,
                stderr: error.stderr
            };
        }
    }
    
    /**
     * Simulate factory action through Haskell
     */
    async simulateFactoryAction(action, params) {
        console.log(`🔧 Simulating Factory Action: ${action}`);
        console.log('📋 Parameters:', JSON.stringify(params, null, 2));
        
        // For demo purposes, we'll simulate the Haskell execution
        const simulationResult = {
            success: true,
            action: action,
            params: params,
            timestamp: new Date().toISOString(),
            txHash: crypto.createHash('sha256')
                .update(action + JSON.stringify(params) + Date.now())
                .digest('hex'),
            escrowAddress: this.generateAddress(`${action}_${params.orderHash || 'demo'}`)
        };
        
        console.log('✅ Simulation Result:', simulationResult);
        return simulationResult;
    }
    
    /**
     * Check if current time allows for a specific action
     */
    checkTimeWindow(timelocks, action, currentTime = null) {
        const now = currentTime || Math.floor(Date.now() / 1000);
        
        switch (action) {
            case 'SrcWithdraw':
                return now >= timelocks.srcWithdrawTime && now < timelocks.srcPublicWithdrawTime;
            case 'SrcPublicWithdraw':
                return now >= timelocks.srcPublicWithdrawTime && now < timelocks.srcCancelTime;
            case 'SrcCancel':
                return now >= timelocks.srcCancelTime && now < timelocks.srcPublicCancelTime;
            case 'SrcPublicCancel':
                return now >= timelocks.srcPublicCancelTime;
            case 'DstWithdraw':
                return now >= timelocks.dstWithdrawTime && now < timelocks.dstPublicWithdrawTime;
            case 'DstPublicWithdraw':
                return now >= timelocks.dstPublicWithdrawTime && now < timelocks.dstCancelTime;
            case 'DstCancel':
                return now >= timelocks.dstCancelTime;
            default:
                return false;
        }
    }
    
    /**
     * Get wallet address from environment
     */
    getWalletAddress() {
        // Extract from seed phrase or use a demo address
        return process.env.CARDANO_WALLET_ADDRESS || this.generateAddress('demo_wallet');
    }
    
    /**
     * Display transaction summary
     */
    displayTxSummary(action, result, params = {}) {
        console.log('\n🎯 TRANSACTION SUMMARY');
        console.log('======================');
        console.log(`Action: ${action}`);
        console.log(`Status: ${result.success ? '✅ SUCCESS' : '❌ FAILED'}`);
        console.log(`Timestamp: ${new Date().toLocaleString()}`);
        console.log(`Network: ${this.network.toUpperCase()}`);
        
        if (result.success) {
            console.log(`Transaction Hash: ${result.txHash}`);
            if (result.escrowAddress) {
                console.log(`Escrow Address: ${result.escrowAddress}`);
            }
        } else {
            console.log(`Error: ${result.error}`);
        }
        
        if (Object.keys(params).length > 0) {
            console.log('\n📋 Parameters:');
            Object.entries(params).forEach(([key, value]) => {
                console.log(`  ${key}: ${value}`);
            });
        }
        
        console.log('======================\n');
    }
}

module.exports = CardanoSwapUtils;
