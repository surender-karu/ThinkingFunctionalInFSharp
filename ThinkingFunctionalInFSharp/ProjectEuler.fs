namespace ThinkingFunctionalInFSharp

(* Solutions to problems from ProjectEuler.net *)
module ProjectEuler =
    (* Problem #1 - Multiples of 3 & 5. Ans. 233168 *)
    let multiples3and5 = 
        let n = 1000
        Seq.init 1000 (fun i -> if i % 3 = 0 || i % 5 = 0 then i else 0)
        |> Seq.sum
    
    (* Problem #2 - Sum Even Fibonacci numbers bellow 4 million. Ans. 4613732 *)
    let evenFibonacci =
        let getFibSeq maxValue = 
            Seq.unfold (fun state -> 
                            if (snd state > maxValue) then None
                            else Some(fst state + snd state, (snd state, fst state + snd state))) (0,1)
         
        getFibSeq 4000000
        |> Seq.filter (fun i -> i % 2 = 0)
        |> Seq.sum

    (* Problem #3 - Find the largest prime factor of a composite number. Ans. 6857 *)
    let largestPrimeFactor = 
        let mutable num = 600851475143I

        let mutable lastFactor = 1I

        if num % 2I = 0I then
            lastFactor <- 2I
            num <- num / 2I
            while num % 2I = 0I do
                num <- num / 2I
        else
            lastFactor <- 1I
        
        let mutable factor = 3I

        // the devisor cannot be larger than the sqrt of num
        let mutable maxFactor = bigint (sqrt (float(num)))
        while num > 1I && factor <= maxFactor do
            if num % factor = 0I then 
                lastFactor <- factor
                num <- num / factor
                while num % factor = 0I do
                    num <- num / factor
                maxFactor <- bigint (sqrt  (float(num)))
            factor <- factor + 2I
        if num = 1I then lastFactor else num
    
    (* Problem #4 -  largest palindrome made from the product of two 3-digit numbers. Ans: 906609 *)
    let findLargestPalindrome =
        let isPalindrome n = 
            let rec getCoef n accum =
                let r = n % 10
                let d = n / 10
                match d with
                | 0 -> accum@[r]
                | _ -> (getCoef d accum)@[r]            
            
            let (reversed, _) = 
                getCoef n []
                |> List.fold (fun (accum, powTen) state -> (accum + (state * powTen), powTen * 10)) (0, 1)
            
            n = reversed
        
        let mutable currentNum = 0
        let mutable largestPalindrome = 0
        let mutable i = 100
        let mutable j = 100
        while i >= 100 do
            j <- i
            while j <= 999 do
                currentNum <- i * j
                if (isPalindrome currentNum) && currentNum > largestPalindrome then
                    largestPalindrome <- currentNum
                j <- j + 1
            i <- i + 1
        largestPalindrome
        
        
        

