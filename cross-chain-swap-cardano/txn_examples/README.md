# Cross-Chain Swap Cardano - Transaction Examples

This directory contains interaction scripts for the cross-chain swap protocol on Cardano. These scripts demonstrate how to deploy, interact with, and manage atomic swap escrows for cross-chain transactions.

## 📋 Overview

The cross-chain swap protocol enables trustless atomic swaps between Cardano and other blockchains using Hash Time Lock Contracts (HTLCs). The protocol consists of:

- **Source Escrows**: Lock funds when swapping FROM Cardano to another chain
- **Destination Escrows**: Lock funds when swapping TO Cardano from another chain
- **EscrowFactory**: Factory contract for deploying escrows with proper integration

## 🔧 Setup

1. Ensure you're in the cross-chain-swap-cardano directory:
   ```bash
   cd cross-chain-swap-cardano
   ```

2. Install Node.js dependencies:
   ```bash
   npm install
   ```

3. Make sure your `.env` file is configured with:
   ```
   CARDANO_SEED_PHRASE="your seed phrase"
   BLOCKFROST_API_KEY="your blockfrost api key"
   BLOCKFROST_API_BASE_URL="https://cardano-preprod.blockfrost.io/api/v0"
   ```

## 📝 Available Scripts

### Core Scripts

| Script | Purpose | Usage |
|--------|---------|-------|
| `utils.js` | Utility functions for all scripts | Library only |
| `deploy-escrow-src.js` | Deploy source escrow (Cardano → Other) | `node deploy-escrow-src.js` |
| `deploy-escrow-dst.js` | Deploy destination escrow (Other → Cardano) | `node deploy-escrow-dst.js [orderHash] [srcCancelTime]` |
| `withdraw-src.js` | Withdraw from source escrow | `node withdraw-src.js [secret] [target]` |
| `withdraw-dst.js` | Withdraw from destination escrow | `node withdraw-dst.js [secret]` |
| `cancel-src.js` | Cancel source escrow | `node cancel-src.js [refundTarget]` |
| `cancel-dst.js` | Cancel destination escrow | `node cancel-dst.js [refundTarget]` |
| `demo-full-swap.js` | Complete atomic swap demo | `node demo-full-swap.js [mode]` |

### Demo Script Modes

```bash
# Complete atomic swap demonstration
node demo-full-swap.js full

# Deploy escrows only
node demo-full-swap.js deploy-only

# Withdraw from existing escrows
node demo-full-swap.js withdraw-only

# Cancellation demonstration
node demo-full-swap.js cancel-demo

# Interactive menu
node demo-full-swap.js interactive
```

## 🔄 Typical Workflows

### 1. Complete Cross-Chain Swap

```bash
# Step 1: Alice deploys source escrow (locks ADA on Cardano)
node deploy-escrow-src.js

# Step 2: Bob deploys destination escrow (locks tokens on other chain)
node deploy-escrow-dst.js

# Step 3: Alice withdraws from destination (reveals secret)
node withdraw-src.js

# Step 4: Bob withdraws from source using revealed secret
node withdraw-dst.js
```

### 2. Standalone Escrow Testing

```bash
# Deploy individual escrows for testing
node deploy-escrow-src.js
node deploy-escrow-dst.js standalone_order_hash 1234567890

# Test withdrawals
node withdraw-src.js
node withdraw-dst.js your_secret_here
```

### 3. Cancellation Flow

```bash
# Deploy escrows
node deploy-escrow-src.js
node deploy-escrow-dst.js

# Cancel if swap needs to be aborted
node cancel-src.js
node cancel-dst.js
```

## ⏰ Timelock System

The scripts use a simplified timelock system with these default windows:

| Action | Window | Duration | Who Can Execute |
|--------|--------|----------|-----------------|
| Src Withdraw | 0-5 min | 5 min | Taker only |
| Src Public Withdraw | 5-10 min | 5 min | Anyone |
| Src Cancel | 10-15 min | 5 min | Taker only |
| Src Public Cancel | 15+ min | ∞ | Anyone |
| Dst Withdraw | 0-7.5 min | 7.5 min | Maker only |
| Dst Public Withdraw | 7.5-12.5 min | 5 min | Anyone |
| Dst Cancel | 12.5+ min | ∞ | Taker/Anyone |

## 💰 Default Parameters

The scripts use these default values (customizable in `utils.js`):

- **Swap Amount**: 5 ADA (5,000,000 lovelace)
- **Safety Deposit**: 2 ADA (2,000,000 lovelace)
- **Network**: Cardano Preprod Testnet
- **Factory Address**: `addr_test1w9fbe9d3e7b9cf25f6b36d25ae170beb8a6ac8088a08ebd03311d`
- **LOP Integration**: `addr_test1wp203846488426adefdc379e46cc9713d1695f5dd424728694acb07f1e`

## 📊 Script Outputs

Each script generates deployment/transaction files:

- `src-escrow-deployment.json` - Source escrow details
- `dst-escrow-deployment.json` - Destination escrow details

These files contain:
- Escrow addresses
- Order hashes and secrets
- Participant addresses
- Timelock configurations
- Transaction hashes
- Current status

## 🔐 Security Notes

⚠️ **Important Security Considerations**:

1. **Secrets**: The demo scripts save secrets to JSON files for convenience. In production:
   - Never store secrets in plain text
   - Use secure key management
   - Secrets should only be revealed when withdrawing

2. **Test Environment**: These scripts are for Cardano Preprod testnet only
   - Use test ADA only
   - Not suitable for mainnet without modifications

3. **Address Generation**: Demo addresses are generated for testing
   - Use proper wallet integration for production
   - Ensure proper key derivation

## 🎯 Integration with Ethereum

These Cardano scripts are designed to work with the Ethereum cross-chain-swap contracts:

- **Order Hashes**: Compatible format for cross-chain linking
- **Secrets**: Same hashing algorithm (SHA-256)
- **Timelock Logic**: Coordinated timing for atomic execution
- **Error Handling**: Graceful failure modes

## 🚀 Quick Start

Run the complete demo:

```bash
# Navigate to txn_examples directory
cd txn_examples

# Run interactive demo
node demo-full-swap.js interactive

# Or run full automated demo
node demo-full-swap.js full
```

This will demonstrate the complete cross-chain atomic swap flow with detailed logging and status updates.

## 🐛 Troubleshooting

**Common Issues**:

1. **"Could not load deployment file"**: Run the deployment script first
2. **"Timelock not active"**: Wait for the appropriate time window
3. **"Invalid secret"**: Ensure the secret matches the original hash
4. **"Not authorized"**: Check if you're calling from the correct address

**Debug Mode**: Set `DEBUG=true` in your environment for verbose logging.

## 📚 Further Reading

- [Cross-Chain Swap Protocol Documentation](../README.md)
- [Cardano Smart Contracts Guide](../src/Contracts/)
- [Timelock System Details](../src/Lib/TimelocksLib.hs)
- [Factory Pattern Implementation](../src/Contracts/EscrowFactory.hs)
