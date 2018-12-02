namespace ThinkingFunctionalInFSharp

open System

open StatisticalFunctions

module MCEuropianOptionPricing = 
    // Generate random numbers from a standard normal distribution.
    // The accuracy of the calculation depends on the randomness of the number generator used.
    let getRandomNumbers num = 
        let rnd = new Random()
        List.init num (fun _ -> normalInv (rnd.NextDouble()))

    /// Price a European option with 
    /// Spot - s0, Strike - k, Time - t, Rate - r, volatility - v
    let priceEuropeanOption s0 k t r v =
        let paths = 100000
        let dist = getRandomNumbers paths

        let mean = dist       
                    |> List.map (fun z -> 
                            let st = s0 * exp ((r - 0.5 * v ** 2.0) * t + v * z * (sqrt t))
                            max (st - k) 0.0
                            )
                    |> List.average
        exp(-r * t) * mean