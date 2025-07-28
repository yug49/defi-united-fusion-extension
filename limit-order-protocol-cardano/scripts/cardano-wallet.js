#!/usr/bin/env node

// Cardano wallet utilities
const crypto = require('crypto');
const bech32 = require('bech32');

// Function to derive private key from seed phrase
function derivePrivateKeyFromSeed(seedPhrase) {
    // This is a simplified derivation - in production you'd use proper BIP39/BIP32
    const hash = crypto.createHash('sha256').update(seedPhrase).digest();
    return hash.toString('hex');
}

// Function to create Cardano address from private key
function createCardanoAddress(privateKey, isTestnet = true) {
    // Simplified address generation - in production use proper Cardano libraries
    const pubKeyHash = crypto.createHash('sha256').update(privateKey, 'hex').digest();
    const addressBytes = Buffer.concat([
        Buffer.from([isTestnet ? 0x00 : 0x01]), // Network tag
        pubKeyHash.slice(0, 28) // First 28 bytes as credential
    ]);
    
    const prefix = isTestnet ? 'addr_test' : 'addr';
    const words = bech32.bech32.toWords(addressBytes);
    return bech32.bech32.encode(prefix, words);
}

// Read environment variables
const seedPhrase = process.env.CARDANO_SEED_PHRASE;
if (!seedPhrase) {
    console.error('CARDANO_SEED_PHRASE not found in environment');
    process.exit(1);
}

// Generate credentials
const privateKey = derivePrivateKeyFromSeed(seedPhrase);
const address = createCardanoAddress(privateKey, true); // true for testnet

console.log('Cardano Credentials Generated:');
console.log('================================');
console.log('Private Key:', privateKey);
console.log('Address:', address);
console.log('Network: Preprod Testnet');

// Export for use in deployment scripts
module.exports = {
    privateKey,
    address,
    seedPhrase
};
