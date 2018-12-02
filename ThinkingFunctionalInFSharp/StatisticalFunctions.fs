namespace ThinkingFunctionalInFSharp

open System

module StatisticalFunctions =
    /// Mean of a population or sample.
    let mean lst =         
        lst 
        |> List.average
    
    /// Population variance.
    let variance lst = 
        let avg = mean lst
        let (sum, length) = 
            lst 
            |> List.fold (fun (accum, len) state -> (accum + (state - avg) ** 2.0 , len + 1.0)) (0.0, 0.0)
        sum / length

    /// Sample variance.
    let variance_sample lst = 
        let avg = mean lst
        let (sum, length) = 
            lst 
            |> List.fold (fun (accum, len) state -> (accum + (state - avg) ** 2.0 , len + 1.0)) (0.0, 0.0)
        sum / (length - 1.0)
   
    /// The population Standard deviation.
    let std lst =
        sqrt (variance lst)
    
    /// The sample standard deviation.
    let std_sample lst = 
        sqrt (variance_sample lst)
    
    // Population Covariance of to distibutions. 
    // The lenght of the lists are not validated to be same.
    let covariance (x:float list) (y:float list) =
        let meanX = mean x
        let meanY = mean y
        let (sum, length) = (List.fold2 (fun (accum, len) xi yi -> (accum + (xi - meanX) * (yi - meanY), len + 1.0)) (0.0, 0.0) x y)
        sum / length

    // Sample Covariance of to distibutions. 
    // The lenght of the lists are not validated to be same.
    let covariance_sample (x:float list) (y:float list) =
        let meanX = mean x
        let meanY = mean y
        let (sum, length) = (List.fold2 (fun (accum, len) xi yi -> (accum + (xi - meanX) * (yi - meanY), len + 1.0)) (0.0, 0.0) x y)
        sum / (length - 1.0)
    
    // The correlation between two distributions.
    let correlation (x:float list) (y:float list) =
        let meanX = mean x
        let meanY = mean y
        let (sum, length) = (List.fold2 (fun (accum, len) xi yi -> (accum + (xi - meanX) * (yi - meanY), len + 1.0)) (0.0, 0.0) x y)
        (sum / length) / ((std x) * (std y))

    // The error function erf 
    // http://mathworld.wolfram.com/Erf.html
    let erf x =
        if (Double.IsNegativeInfinity x) then -1.0
        elif (Double.IsPositiveInfinity x) then 1.0
        else
            let z = abs x
            let t = 1.0 / (1.0 + 0.5 * z) 
            let r = t * exp (-z * z - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277))))))))) 
            if (x >= 0.0) then 1.0 - r else r - 1.0

    // The complementary error function.
    // http://mathworld.wolfram.com/Erfc.html
    let erfc x = 
        if (Double.IsNegativeInfinity x) then 2.0
        elif (Double.IsPositiveInfinity x) then 0.0
        else
            1.0 - erf x

    // The inverse error function.
    // http://mathworld.wolfram.com/Erfi.html.
    // Coefficients of the polinomial approximation built using.
    // formula in https://en.wikipedia.org/wiki/Error_function#Inverse_functions.
    let erfi y = 
        if y < -1.0 || y > 1.0 then
            failwith "Inverse error function is only valid between [-1.0, 1.0]"
        elif y = -1.0 then Double.NegativeInfinity
        elif y = 1.0 then Double.PositiveInfinity
        else
            let n = 25
            let c = Array.init n (fun i -> if i <= 1 then 1.0 else 0.0)
            [0..(n - 1)]
            |> List.map (fun k ->
                            if k > 1 then 
                                c.[k] <- [0..(k - 1)] 
                                            |> List.map (fun m -> (c.[m] * c.[k - 1 - m]) / ((float(m) + 1.0)  * (2.0 * float(m) + 1.0)))
                                            |> List.sum
                            let p = 2.0 * float(k) + 1.0
                            Math.Pow(y * 0.5 * sqrt (Math.PI), p) * c.[k] / p
                        )
            |> List.sum

    // The inverse complementary error function.
    // http://mathworld.wolfram.com/Erfi.html.
    // Coefficients of the polinomial approximation built using.
    let erfci y = 
        if y < 0.0 || y > 2.0 then
            failwith "Inverse complementary error function is only valid between [0.0, 2.0]"
        elif y = 0.0 then Double.PositiveInfinity
        elif y = 2.0 then Double.NegativeInfinity
        else
            erfi (1.0 - y)
    
    // CDF of a standard normal distribution
    let normalCdf t = 
        (erfci (-t / (sqrt 2.0))) / 2.0
    
    // PDF of a standard normal distribution
    let normalPdf t =
        exp (-(t * t / 2.0)) / sqrt (2.0 * Math.PI)
    
    // Gets the inverse value of a normal distribution at a given point.
    let normalInv p = 
        (erfci (2.0 * p)) * -(sqrt 2.0)