# RateLab
_Providing Rate Traders The Necceasary Tools For Managing Exposure and Risk In The United States Makrkets_

## About RateLab

## About RLtools
RLtools is the supporting function package for the RateLab ShinyApp built in R. It provides several tools for forecasting, risk managagement, and portfolio construction. Provided is a list of popular functions available.

### Bond Pricing Tool (BOND)
The BOND function calculates various bond-related metrics, such as price, yield, coupon rate, and face value, based on user input.

**Parameters**
+ price: The price of the bond.
+ yield: The current yield of the bond.
+ coupon_rate: The annualized coupon rate.
+ maturity: Years to maturity.
+ face_value: The bond's face value (default: 100).
+ solve_for: The variable to solve for (price, yield, coupon_rate, face_value).

**Example Usage**
```
BOND(yield = 0.05, coupon_rate = 0.06, maturity = 3, solve_for = "price")
BOND(price = 95, coupon_rate = 0.06, maturity = 3, solve_for = "yield")
```

### Bootstrap Yield Curve from Bond Prices (BOOTSTRAP_B)
This function bootstraps a zero-coupon yield curve from known bond prices and coupon rates, assuming annual coupons.

**Parameters**
+ bond_data: A data frame containing bond prices, coupon rates, maturities, and face values.

**Example Usage**
```
bond_data <- data.frame(
    Maturity = c(1, 2, 3),
    Price = c(950, 920, 880),
    Coupon = c(0, 50, 60),
    FaceValue = 1000
)
BOOTSTRAP_B(bond_data)
```

### Bootstrap Yield Curve from Swap Rates (BOOTSTRAP_S)
This function bootstraps a zero-coupon yield curve from swap rates, assuming annualized par rates.

**Parameters**
+ data: A data frame containing swap rates and maturities.

**Example Usage**
```
swap_data <- tibble(maturity = c(1, 2, 3, 4),
                    rate = c(0.03, 0.035, 0.04, 0.045))
BOOTSTRAP_S(data = swap_data)
```

### Cox-Ingersoll-Ross (CIR) Interest Rate Simulation (CIR)
Simulates the CIR model, commonly used in fixed income pricing, ensuring that interest rates remain non-negative.

**Parameters**
+ LT_mean: Long-term mean interest rate.
+ rate: Initial interest rate.
+ delta_T: Time step.
+ sigma: Interest rate volatility.
+ theta: Mean reversion speed.
+ T2M: Time to maturity (total time horizon).
+ nsims: Number of simulation paths.

**Example Usage**
```
CIR(LT_mean = 0.05, rate = 0.03, delta_T = 1/12, sigma = 0.02, theta = 0.1, T2M = 1, nsims = 5)
```

BDT(volatility = c(0.01, 0.015, 0.02), spot_rates = c(0.03, 0.035, 0.04))

These functions provide a comprehensive toolkit for bond pricing, yield curve construction, and interest rate modeling, enabling robust financial analysis in R.

