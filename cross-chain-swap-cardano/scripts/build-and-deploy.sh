#!/bin/bash

echo "🚀 CROSS-CHAIN SWAP CARDANO BUILD & DEPLOY"
echo "==========================================="
echo ""

# Set script to exit on any error
set -e

# Check if Node.js dependencies are installed
echo "📦 Checking Node.js dependencies..."
if [ ! -d "node_modules" ]; then
    echo "Installing Node.js dependencies..."
    npm install
else
    echo "✅ Node.js dependencies found"
fi
echo ""

# Check Haskell environment
echo "🔧 Checking Haskell environment..."
if ! command -v cabal &> /dev/null; then
    echo "❌ Cabal not found! Please install GHC and Cabal first."
    exit 1
fi
echo "✅ Cabal found"
echo ""

# Clean previous builds
echo "🧹 Cleaning previous builds..."
cabal clean
rm -f *.o *.hi
rm -rf src/**/*.o src/**/*.hi
rm -rf build_temp
echo "✅ Cleanup completed"
echo ""

# Build the project
echo "🔨 Building Haskell project..."
cabal build
if [ $? -ne 0 ]; then
    echo "❌ Build failed!"
    exit 1
fi
echo "✅ Build successful"
echo ""

# Run tests
echo "🧪 Running tests..."
cabal test
if [ $? -ne 0 ]; then
    echo "❌ Tests failed!"
    exit 1
fi
echo "✅ All tests passed"
echo ""

# Check for wallet configuration
if [ ! -f ".env" ]; then
    echo "🔐 No wallet found. Generating new wallet..."
    node scripts/cardano-wallet.js
    echo ""
    echo "⚠️  IMPORTANT: Fund the generated address with test ADA and get Blockfrost API key!"
    echo "Then re-run this script to deploy."
    exit 0
fi

# Load environment variables
if [ -f ".env" ]; then
    export $(cat .env | grep -v '#' | xargs)
fi

# Check required environment variables
if [ -z "$CARDANO_WALLET_ADDRESS" ] || [ -z "$LIMIT_ORDER_PROTOCOL_ADDRESS" ]; then
    echo "❌ Missing required environment variables!"
    echo "Please run: node scripts/cardano-wallet.js first"
    exit 1
fi

echo "📋 Environment Configuration:"
echo "   Wallet: $CARDANO_WALLET_ADDRESS"
echo "   LOP Contract: $LIMIT_ORDER_PROTOCOL_ADDRESS"
echo "   Network: ${CARDANO_NETWORK:-preprod}"
echo ""

# Deploy the contracts
echo "🚀 Starting deployment..."
node scripts/deploy.js

if [ $? -eq 0 ]; then
    echo ""
    echo "🎉 DEPLOYMENT COMPLETED SUCCESSFULLY!"
    echo ""
    echo "📄 Deployment artifacts:"
    echo "   deployment-result.json - Deployment details"
    echo "   plutus-script.json - Plutus script information"
    echo "   .env - Wallet and configuration"
    echo ""
    echo "✅ Cross-Chain Swap EscrowFactory is ready!"
else
    echo "❌ Deployment failed!"
    exit 1
fi
