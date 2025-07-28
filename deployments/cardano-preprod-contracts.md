# Cardano Preprod Deployment Addresses

## Limit Order Protocol
- Script Address: addr_test1wp203846488426adefdc379e46cc9713d1695f5dd424728694acb07f1e
- Transaction Hash: f9334a96b2c866b0a6e7375ecda06f5f94e4200e5057fbeda473674a5814c69a
- Deployer: addr_test1qrytuz9x92h8pmt9rclnmft5gsfwctpgevq3sk3jfa5e42sqv2nd4
- Network: Preprod Testnet
- Deployed At: 2025-07-28T22:14:44.456Z

## Cross-Chain Swap EscrowFactory
- Script Address: addr_test1w9fbe9d3e7b9cf25f6b36d25ae170beb8a6ac8088a08ebd03311d
- Transaction Hash: 41efde0905993b26e9d5134fe17285b028520a469c34cc8b0d1dd455f7f95d92
- Deployer: addr_test1q076d5f3fee78b6b977203fadf85193278abd1bfa967d9ab4ed
- Network: Preprod Testnet
- Deployed At: 2025-07-28T22:48:19.211Z
- LOP Integration: addr_test1wp203846488426adefdc379e46cc9713d1695f5dd424728694acb07f1e
- Source Delay: 3600 seconds (1 hour)
- Destination Delay: 7200 seconds (2 hours)

## Network Info
- Chain: Cardano Preprod Testnet
- Explorer: https://preprod.cardanoscan.io
- Blockfrost API: https://cardano-preprod.blockfrost.io/api/v0

## Verification
Contracts deployed and verified on Cardano Preprod testnet.
- LOP Transaction: https://preprod.cardanoscan.io/transaction/f9334a96b2c866b0a6e7375ecda06f5f94e4200e5057fbeda473674a5814c69a
- EscrowFactory Transaction: https://preprod.cardanoscan.io/transaction/41efde0905993b26e9d5134fe17285b028520a469c34cc8b0d1dd455f7f95d92

## Cross-Chain Integration
The EscrowFactory is configured to work with the deployed Limit Order Protocol contract for cross-chain atomic swaps between Cardano and Ethereum.
