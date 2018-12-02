namespace ThinkingFunctionalInFSharp

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

    let covariance (x:float list) (y:float list) =
        let meanX = mean x
        let meanY = mean y
        let (sum, length) = (List.fold2 (fun (accum, len) xi yi -> (accum + (xi - meanX) * (yi - meanY), len + 1.0)) (0.0, 0.0) x y)
        sum / length

    let covariance_sample (x:float list) (y:float list) =
        let meanX = mean x
        let meanY = mean y
        let (sum, length) = (List.fold2 (fun (accum, len) xi yi -> (accum + (xi - meanX) * (yi - meanY), len + 1.0)) (0.0, 0.0) x y)
        sum / (length - 1.0)
    
    let correlation (x:float list) (y:float list) =
        let meanX = mean x
        let meanY = mean y
        let (sum, length) = (List.fold2 (fun (accum, len) xi yi -> (accum + (xi - meanX) * (yi - meanY), len + 1.0)) (0.0, 0.0) x y)
        (sum / length) / ((std x) * (std y))