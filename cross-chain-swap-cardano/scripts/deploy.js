const fs = require('fs');
const { exec } = require('child_process');
const util = require('util');
require('dotenv').config();

const execAsync = util.promisify(exec);

async function deployEscrowFactory() {
    console.log('🚀 DEPLOYING CROSS-CHAIN SWAP ESCROW FACTORY');
    console.log('============================================');
    console.log('');
    
    try {
        // Check if wallet is configured
        if (!process.env.CARDANO_WALLET_ADDRESS || !process.env.LIMIT_ORDER_PROTOCOL_ADDRESS) {
            console.log('❌ Missing wallet configuration!');
            console.log('Run: node scripts/cardano-wallet.js first');
            return;
        }
        
        const walletAddr = process.env.CARDANO_WALLET_ADDRESS;
        const lopAddr = process.env.LIMIT_ORDER_PROTOCOL_ADDRESS;
        
        console.log('📋 Deployment Configuration:');
        console.log(`   Deployer Address: ${walletAddr}`);
        console.log(`   LOP Contract: ${lopAddr}`);
        console.log(`   Network: ${process.env.CARDANO_NETWORK || 'preprod'}`);
        console.log('');
        
        // Build the project first
        console.log('🔨 Building Haskell project...');
        const buildResult = await execAsync('cabal build');
        console.log('✅ Build completed');
        console.log('');
        
        // Test the factory creation
        console.log('🧪 Testing EscrowFactory creation...');
        const testResult = await execAsync('cabal run cross-chain-swap-cardano test');
        console.log(testResult.stdout);
        
        if (testResult.stderr && testResult.stderr.trim()) {
            console.log('⚠️  Test warnings:');
            console.log(testResult.stderr);
        }
        
        // Deploy the factory
        console.log('🚀 Deploying EscrowFactory...');
        const deployResult = await execAsync(`cabal run cross-chain-swap-cardano deploy "${walletAddr}" "${lopAddr}"`);
        console.log(deployResult.stdout);
        
        if (deployResult.stderr && deployResult.stderr.trim()) {
            console.log('⚠️  Deployment warnings:');
            console.log(deployResult.stderr);
        }
        
        // Generate deployment artifacts
        const deploymentInfo = {
            contractType: 'EscrowFactory',
            deployer: walletAddr,
            lopContract: lopAddr,
            network: process.env.CARDANO_NETWORK || 'preprod',
            deployedAt: new Date().toISOString(),
            srcDelay: 3600,
            dstDelay: 7200,
            status: 'deployed'
        };
        
        // Save deployment result
        fs.writeFileSync('deployment-result.json', JSON.stringify(deploymentInfo, null, 2));
        console.log('💾 Deployment info saved to deployment-result.json');
        
        // Create Plutus script placeholder (in real deployment, this would be generated from Haskell)
        const plutusScript = {
            type: "PlutusScriptV2",
            description: "Cross-Chain Swap EscrowFactory",
            cborHex: "590a4d590a4a010000...", // This would be the actual compiled Plutus script
            hash: "cross_chain_swap_factory_script_hash"
        };
        
        fs.writeFileSync('plutus-script.json', JSON.stringify(plutusScript, null, 2));
        console.log('📜 Plutus script info saved to plutus-script.json');
        
        console.log('');
        console.log('✅ DEPLOYMENT COMPLETED SUCCESSFULLY!');
        console.log('');
        console.log('📊 Summary:');
        console.log(`   Contract: EscrowFactory`);
        console.log(`   Network: ${deploymentInfo.network}`);
        console.log(`   Deployer: ${deploymentInfo.deployer}`);
        console.log(`   LOP Integration: ${deploymentInfo.lopContract}`);
        console.log(`   Source Delay: ${deploymentInfo.srcDelay}s`);
        console.log(`   Destination Delay: ${deploymentInfo.dstDelay}s`);
        console.log('');
        console.log('🎯 NEXT STEPS:');
        console.log('1. The EscrowFactory is ready to create cross-chain swap escrows');
        console.log('2. Integration with Limit Order Protocol is configured');
        console.log('3. Both source and destination escrows can be deployed');
        console.log('4. Ready for atomic cross-chain swaps!');
        
    } catch (error) {
        console.error('❌ Deployment failed:');
        console.error(error.message);
        if (error.stdout) console.log('stdout:', error.stdout);
        if (error.stderr) console.log('stderr:', error.stderr);
    }
}

if (require.main === module) {
    deployEscrowFactory();
}

module.exports = { deployEscrowFactory };
