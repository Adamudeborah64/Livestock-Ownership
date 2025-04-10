# Livestock Ownership & Trade System

A blockchain-based solution for managing livestock ownership, vaccination records, and trading using Clarity smart contracts.

## Features

- Register new livestock with unique identifiers
- Track ownership and transfer history
- Record and verify vaccination history
- Enable secure buying and selling of livestock
- Query animal details and medical records

## Contract Functions

### Administrative Functions
- `register-animal`: Register a new animal with breed, birth date, and initial price
- `add-vaccination-record`: Add a new vaccination record for an animal

### Owner Functions
- `set-sale-status`: List/delist animal for sale and set price
- `transfer-ownership`: Transfer animal ownership (requires payment)

### Read-Only Functions
- `get-animal-details`: Get complete details of an animal
- `get-vaccination-history`: Get vaccination records
- `get-owner`: Get current owner of an animal

## Usage

1. Deploy the contract using Clarinet
2. Register animals using `register-animal`
3. Add vaccination records with `add-vaccination-record`
4. List animals for sale using `set-sale-status`
5. Transfer ownership using `transfer-ownership`

## Requirements

- Clarinet
- Stacks blockchain wallet
```
