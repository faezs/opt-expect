
# Chapter 1

    Reinforcement learning is learning how to map situations to actions to maximize a numerical reward signal.
    The learner must discover which actions yield the highest reward by trying them out. Generally, the action
    taken affects not only the reward but also the next situations and therefore all the future rewards.
    So the two defining characteristics of RL are:
        - Trial and Error Search
        - Delayed Reward

    RL itself denotes 3 things:
        - A type of problem
        - A class of solutions to that type of problem
        - The field that studies those other two

    Formally, RL is:
        - The optimal control of incompletely-known Markov decision processes
    In order to accomplish this goal, RL agents must
        - Sense the state of the environment
        - Act to affect the enviroment
        - Have goals as to what the state of the environment should be
    
    Additionally, RL is different from unsupervised and supervised learning, because the former is about uncovering
    hidden structure in unlabelled data, while the latter is about learning how to extrapolate a particular labelling
    on training data to previously unknown data. While both can function as components in an RL system, neither by
    themselves enable a reward signal from the environment to be maximized.

    A major challenge in RL is the explore-exploit trade-off. To obtain high rewards, an agent must act in ways that
    it has previously learned are good for getting reward. But in order to find these actions, it must try things
    it hasn't tried before. It therefore needs to try a bunch of actions and progressively favor those which are high-reward.

    To solve the ENTIRE task of a goal-directed agent interacting with an uncertain environment,
    we start with a complete, interactive, goal-seeking agent. All RL agents have xplicit goals, can observe their
    enviroment either fully or partially, and can choose actions to influence said environment. It is assumed that 
    the agent MUST act despite significant uncertainty about the environment. When the agent is a planning agent, it 
    must balance planning and choosing actions in real-time,
    as well as improving its understanding of the environment model that it bases its decisions off of.


## Elements of a Reinforcement Learning System
### Agent

    - Observes State
    - Uses Value Function based Policy to Select Action
    - Observers Reward
    - Updates Value Function and Policy based on reward

### Enviroment

    Markov Decision Process, Partially or Fully Observable

### Policy

    An agent's policy is its behaviour at a given time. It is a mapping from the observed states of the 
    enviroment to the actions to be taken in response to a particular observation. It can either be a simple 
    function or a lookup table, or a sophisticated search procedure.
    It may be either deterministic or stochastic.

### Reward Signal
    
    The reward signal defines what the goal is. It is a single number, received by the agent from the 
    environment. The agent's goal is to maximize the total reward it recieves over the long run. It is what tells
    the agent whether a particular action is good or bad within the context of the environment. It is the 
    primary basis for altering the policy. In general, reward signals are stochastic functions of the state of 
    the environment and the agent's actions.

### Value Function

    A value function differs from the reward in that instead of ascribing the immediate goodness at time t, it 
    specifies what is good in the long run. The value of a state is the total amount of reward an agent can
    expect to accumulate over the future, starting at that state. In that sense it has to incorporate the 
    probability that even a low immediate reward might be followed by high future rewards given certain actions
    in certain states.
    Values are secondary to rewards because they are predictions which wouldn't be possible without a reward 
    signal and which only exist to maximize the achievement of rewards. But our actions are selected to 
    get into states with maximimum value rather than states with maximimum reward because over the long-term, 
    this strategy leads to highest cumulative rewards. However, value-determination is hard. Because they have 
    to be constantly re-estimated from sequences of observations experienced by the agent, whereas rewards are
    given by the environment. Value-estimation is therefore a central problem in practical RL algorithms.

### Model of the Environment

    This is something that allows the inference of how the environment state will evolve in response to 
    actions. They are used for planning, in the sense of deciding what to do by evaluating possible futures 
    before they are experienced. 
  
