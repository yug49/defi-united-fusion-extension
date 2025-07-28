#!/bin/bash

echo "🚀 Cardano Limit Order Protocol Build & Deploy Script"
echo "======================================================"

# Exit on any error
set -e

# Configuration
PROJECT_DIR="/Users/shubhtastic/Documents/EthGlobal/defi-united-fusion-extension/limit-order-protocol-cardano"
ENV_FILE="/Users/shubhtastic/Documents/EthGlobal/defi-united-fusion-extension/.env"

# Change to project directory
cd "$PROJECT_DIR"

# Function to print colored output
print_status() {
    echo "✅ $1"
}

print_error() {
    echo "❌ $1"
}

# Check if environment file exists
if [ ! -f "$ENV_FILE" ]; then
    print_error "Environment file not found: $ENV_FILE"
    exit 1
fi

# Source environment variables
source "$ENV_FILE"

# Validate required environment variables
if [ -z "$CARDANO_SEED_PHRASE" ]; then
    print_error "CARDANO_SEED_PHRASE not found in environment"
    exit 1
fi

if [ -z "$BLOCKFROST_API_KEY" ]; then
    print_error "BLOCKFROST_API_KEY not found in environment"
    exit 1
fi

print_status "Environment variables loaded"

# Clean previous builds
echo "🧹 Cleaning previous builds..."
cabal clean
print_status "Previous builds cleaned"

# Build project
echo "🔨 Building Haskell project..."
if cabal build; then
    print_status "Haskell project built successfully"
else
    print_error "Haskell build failed"
    exit 1
fi

# Run tests
echo "🧪 Running tests..."
if cabal test; then
    print_status "All tests passed"
else
    print_error "Tests failed"
    exit 1
fi

# Deploy to Cardano
echo "🌐 Deploying to Cardano Preprod..."
if node scripts/deploy.js; then
    print_status "Deployment completed successfully"
else
    print_error "Deployment failed"
    exit 1
fi

echo ""
echo "🎉 Build and deployment completed successfully!"
echo "📁 Check the following files for deployment details:"
echo "   - Protocol params: $PROJECT_DIR/deployment/protocol-params.json"
echo "   - Plutus script: $PROJECT_DIR/deployment/plutus-script.json"
echo "   - Deployment result: $PROJECT_DIR/deployment/deployment-result.json"
echo "   - Registry: /Users/shubhtastic/Documents/EthGlobal/defi-united-fusion-extension/deployments/cardano-preprod-contracts.md"
