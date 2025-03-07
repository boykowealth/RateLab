# RateLab
_Providing Rate Traders The Necceasary Tools For Managing Exposure and Risk In The United States Markets_

## About RateLab
By: Brayden Boyko & Mitch Greer (Alberta School of Business, FinTech Stream)

RateLab is a fixed-income portfolio management tool designed for U.S. Treasury rate traders. It offers a comprehensive view of market conditions, interest rate co-dynamics, and portfolio exposure/risk. The application serves as an interactive sandbox for traders to test market shocks and stress scenarios on their portfolio positions.

### Key Features:
+ Portfolio Risk Analysis: Breaks down risk exposures using rate co-dynamics and sensitivity measures.
+ Market Shock Testing: Users can apply custom yield curve shifts and stress-test their positions under different rate environments.
+ P&L Attribution: Implements the Modern Risk Framework using a Taylor series expansion to decompose portfolio P&L into key risk factors.
+ Custom Analytics Development: While RateLab does not include built-in predictive models, it allows users to integrate their own forecasting and analytics within its framework.

### Technical Details:
+ Built as an R Shiny app for an intuitive and interactive user interface.
+ C++ integration ensures real-time calculations for efficient scenario modeling and risk analysis.
+ Leverages advanced mathematical techniques for risk modeling and attribution.
+ This combination of high-speed computation and user-friendly design makes RateLab a powerful tool for traders, portfolio managers, and risk analysts looking to understand, manage, and optimize their fixed-income portfolios.

## About RLtools
RLtools is the supporting function package for the RateLab ShinyApp built in R. It provides several tools for forecasting, risk managagement, and portfolio construction. Provided is a list of popular functions available. Future releases of the application will see the implementation of these functions. Currently `RLtools::TREASURY_US()` is the only function implemented into the app; which provides an efficent way of loading US Constant Maturity Yields into R.

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

## Project Information

**This project is developed and maintained by:**
+ Brayden Boyko (bnboyko@ualberta.ca)
+ Mitch Greer (magreer@ualberta.ca)

In collaberation with the Alberta School of Business honours finance (FinTech) program. The project is avaible under the GNU - General Public License.

