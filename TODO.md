## 19-May-2020

    --Add Advantage Estimates--
      - --rewrite rewards To Go and state value estimation to also in the same scan compute the generalized advantage estimate for a given lambda and gamma--
    - --Implement Minibatching--
    - --Implement Shuffling--
      - --mapM shuffleM over each episode's trajectories--
      - --add shuffling to the valueFnLearn ting. Implement multiple training epochs for value Fn.--
      
## 30-5-2020

    -- --Add PPO objective.--
    -- Get rid of episodes and work with streams directly.


## 1-6-2020
    
    - Reward reduction fold is cancelling out because total - accum means total@t - accum@t whereas we need total@T - accum@t
    - PPO loss has residuals, check.
