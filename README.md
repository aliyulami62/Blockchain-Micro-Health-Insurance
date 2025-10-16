# 🏥 Blockchain Micro-Health Insurance

A decentralized health insurance platform built on Stacks blockchain, designed to provide affordable healthcare coverage for low-income workers through verified treatment providers.

## 🌟 Features

- **💰 Affordable Premiums**: Low monthly premiums designed for workers with limited income
- **🔐 Verified Providers**: Only certified healthcare providers can process claims
- **⚡ Fast Claims Processing**: Smart contract automation for quick claim approvals
- **📊 Transparent Coverage**: All transactions and balances are publicly verifiable
- **🛡️ Secure Payments**: Built-in STX token handling for premiums and payouts

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- STX tokens for testing
- Stacks wallet

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Deploy the contract using Clarinet

```bash
clarinet deploy
```

## 📋 Usage Instructions

### For Members

#### 1. 👤 Enroll as a Member
```clarity
(contract-call? .blockchain-micro-health-insurance enroll-member)
```
- Automatically pays first month's premium
- Receives unique member ID
- Activates coverage immediately

#### 2. 💳 Pay Monthly Premium
```clarity
(contract-call? .blockchain-micro-health-insurance pay-premium)
```
- Must be paid within grace period (30 days)
- Extends coverage for another month
- Maintains active status

#### 3. 📝 Submit Insurance Claim
```clarity
(contract-call? .blockchain-micro-health-insurance submit-claim provider-principal "Treatment Description" amount)
```
- Provider must be verified
- Amount cannot exceed maximum coverage
- Coverage must be active

### For Contract Owner

#### 4. ✅ Add Verified Provider
```clarity
(contract-call? .blockchain-micro-health-insurance add-verified-provider provider-principal "Provider Name")
```

#### 5. ✅ Approve Claims
```clarity
(contract-call? .blockchain-micro-health-insurance approve-claim claim-id)
```

#### 6. ❌ Reject Claims
```clarity
(contract-call? .blockchain-micro-health-insurance reject-claim claim-id)
```

### Read-Only Functions

#### 📊 Check Member Information
```clarity
(contract-call? .blockchain-micro-health-insurance get-member-info member-id)
```

#### 📋 View Claim Details
```clarity
(contract-call? .blockchain-micro-health-insurance get-claim-info claim-id)
```

#### 📈 Contract Statistics
```clarity
(contract-call? .blockchain-micro-health-insurance get-contract-stats)
```

## 💡 Key Concepts

### 🏥 Coverage Rules
- **Grace Period**: 30 days (4,320 blocks) after last premium payment
