# ------------------------------------------------------------------------------
# Bank Account System (R6 OOP Example)
# Demonstrates encapsulation, inheritance, polymorphism, and abstraction.
# ------------------------------------------------------------------------------

# setup
rm(list = ls())
pacman::p_load(R6)

# ------------------------------------------------------------------------------
# 1. Abstract Base Class - Account
# Represents the blueprint for all account types.
# Encapsulation: .owner and .balance are private; accessed only via methods.
# Abstraction: monthly_process() is intentionally left to be overridden.
# ------------------------------------------------------------------------------

Account <- R6Class(
  "Account",
  # ------------------ Private fields ------------------
  private = list(
    .owner = NULL,
    # account owner (private)
    .balance = 0       # account balance (private)
  ),
  
  # ------------------ Active bindings ------------------
  # Read-only accessors for private fields
  active = list(
    owner = function(value) {
      if (missing(value))
        return(private$.owner)
      stop("owner is read-only after initialization.")
    },
    balance = function(value) {
      if (missing(value))
        return(private$.balance)
      stop("balance is read-only; use deposit() / withdraw() methods.")
    }
  ),
  
  # ------------------ Public methods ------------------
  public = list(
    # Constructor
    initialize = function(owner, initial_balance = 0) {
      stopifnot(is.character(owner), length(owner) == 1)
      stopifnot(is.numeric(initial_balance), initial_balance >= 0)
      private$.owner <- owner
      private$.balance <- initial_balance
    },
    
    # Deposit money into the account
    deposit = function(amount) {
      stopifnot(is.numeric(amount), length(amount) == 1)
      if (amount <= 0)
        stop("Deposit amount must be positive.")
      private$.balance <- private$.balance + amount
      invisible(self)
    },
    
    # Withdraw money from the account
    withdraw = function(amount) {
      stopifnot(is.numeric(amount), length(amount) == 1)
      if (amount <= 0)
        stop("Withdrawal amount must be positive.")
      if (amount > private$.balance)
        stop("Insufficient funds.")
      private$.balance <- private$.balance - amount
      invisible(self)
    },
    
    # Retrieve the current balance
    get_balance = function() {
      private$.balance
    },
    
    # Abstract method: to be overridden by subclasses
    monthly_process = function() {
      stop("This method should be overridden in derived classes.")
    }
  )
)

# ------------------------------------------------------------------------------
# 2a. Derived Class - CheckingAccount
# Inherits from Account
# Adds monthly_fee and overrides monthly_process().
# Demonstrates inheritance and polymorphism.
# ------------------------------------------------------------------------------

CheckingAccount <- R6Class(
  "CheckingAccount",
  inherit = Account,
  
  # Private field for monthly fee
  private = list(.monthly_fee = 0),
  
  # Active binding for monthly fee
  active = list(
    monthly_fee = function(value) {
      if (missing(value))
        return(private$.monthly_fee)
      stopifnot(is.numeric(value), value >= 0)
      private$.monthly_fee <- value
    }
  ),
  
  public = list(
    # Constructor
    initialize = function(owner,
                          initial_balance = 0,
                          monthly_fee = 0) {
      super$initialize(owner, initial_balance)
      self$monthly_fee <- monthly_fee
    },
    
    # Override monthly_process(): deduct monthly fee
    monthly_process = function() {
      self$withdraw(self$monthly_fee)
    }
  )
)

# ------------------------------------------------------------------------------
# 2b. Derived Class - SavingsAccount
# Inherits from Account
# Adds interest_rate and overrides monthly_process().
# Demonstrates inheritance and polymorphism.
# ------------------------------------------------------------------------------

SavingsAccount <- R6Class(
  "SavingsAccount",
  inherit = Account,
  
  # Private field for interest rate
  private = list(
    .interest_rate = 0  # percent per month
  ),
  
  # Active binding for interest rate
  active = list(
    interest_rate = function(value) {
      if (missing(value))
        return(private$.interest_rate)
      stopifnot(is.numeric(value), value >= 0)
      private$.interest_rate <- value
    }
  ),
  
  public = list(
    # Constructor
    initialize = function(owner,
                          initial_balance = 0,
                          interest_rate = 0) {
      super$initialize(owner, initial_balance)
      self$interest_rate <- interest_rate
    },
    
    # Override monthly_process(): apply interest
    monthly_process = function() {
      interest <- self$get_balance() * self$interest_rate / 100
      if (interest > 0)
        self$deposit(interest)
    }
  )
)

# ------------------------------------------------------------------------------
# 3. Bank Class
# Manages multiple accounts.
# Demonstrates composition and polymorphism.
# ------------------------------------------------------------------------------

Bank <- R6Class(
  "Bank",
  private = list(
    .accounts = list()  # list of Account objects (keyed by owner name)
  ),
  
  public = list(
    # Add a new account to the bank
    add_account = function(account) {
      stopifnot(inherits(account, "Account"))
      private$.accounts[[account$owner]] <- account
      invisible(self)
    },
    
    # Compute total assets across all accounts
    total_assets = function() {
      Reduce(`+`, lapply(private$.accounts, function(a)
        a$get_balance()), 0)
    },
    
    # Retrieve an account by owner name
    get_account = function(owner) {
      private$.accounts[[owner]]
    },
    
    # Run monthly processing for all accounts
    process_all_accounts = function() {
      lapply(private$.accounts, function(a)
        a$monthly_process())
      invisible(self)
    },
    
    # List all account owners in the bank
    list_owners = function() {
      names(private$.accounts)
    }
  )
)
                