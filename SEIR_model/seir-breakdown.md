# SEIR Model Breakdown

## Model Structure

### Parameters

-   $\epsilon$: Rate of Infection (How long it takes to move from Exposed to Infected)
-   $\omega$: Waning of Immunity
-   $b$:
-   $\mu$: probability of transition given contact
-   $\gamma$: recovery rate for asymptomatic
-   $\alpha$: infection induced death rate
-   $c$: rate of contact between groups
-   $S_s$: scale susceptibility $\rightarrow$ people with immunity are less susceptible
-   $S_i$: scale infectiousness $\rightarrow$ people with immunity are less infectious
-   $\delta$: aging rate

We have 4 age groups:

-   Children (0-18 years) $\rightarrow$ denoted as $C$ in the model
-   Childless Adults (Adults 18-50 without children) $\rightarrow$ denoted as $CA$ in the model
-   Parents (Adults 18-50 with children) $\rightarrow$ denoted as $P$ in the model
-   Seniors (Adults 50+) $\rightarrow$ denoted as $S$ in the model

For each age group, we have parameters:

-   $S$: Susceptible ($S_C, S_{CA}, S_P, S_S$)
-   $E$: Exposed ($E_C, E_{CA}, E_P, E_S$)
-   $I$: Infected ($I_C, I_{CA}, I_P, I_S$)
-   $R$: Recovered ($R_C, R_{CA}, R_P, R_S$)
-   $D$: Dead ($D_C, D_{CA}, D_P, D_S$)

For each of these parameters, we have 2 groups:

-   $1$: no immunity e.g ($S_{C1}, S_{CA1}, S_{P1}, S_{S1}$)
-   $2$: immunity through previous infection or vaccination e.g. ($S_{C2}, S_{CA2}, S_{P2}, S_{S2}$)

### Equations

We model this dynamic using differential equations that model the flow between S, E, I, R, and D and the flow between age groups.

-   $\Delta S_1 = -(\lambda + \text{vacc(t)})S_1 - \delta_{\text{OUT}}S_1 + \delta_{\text{IN}}S_{\text{OTHER}1}$
-   $\Delta E_1 = \lambda S_1 + (\epsilon + \text{vacc(t)})E_1 - \delta_{\text{OUT}}E_1 + \delta_{\text{IN}}E_{\text{OTHER}1}$
-   $\Delta I_1 = \epsilon E_1 - \gamma I_1 - \delta_{\text{OUT}}I_1 + \delta_{\text{IN}}I_{\text{OTHER}1}$
-   $\Delta R_1 = \gamma I_1 - (\omega + \text{vacc(t)} R_1 - \delta_{\text{OUT}}R_1 + \delta_{\text{IN}}R_{\text{OTHER}1}$
-   $\Delta D_1 = \alpha_1 \gamma I_1$
-   $\Delta S_2 = \omega R_1 + b \omega R_2 - (\text{ss} \lambda + \text{vacc(t)})S_2 - \delta_{\text{OUT}}S_2 + \delta_{\text{IN}}S_{\text{OTHER}2}$
-   $\Delta E_2 = \text{ss}\lambda S_2 - (\epsilon + \text{vacc(t)})E_2 - \delta_{\text{OUT}}E_2 + \delta_{\text{IN}}E_{\text{OTHER}2}$
-   $\Delta I_2 = \epsilon E_2 - \gamma I_2 - \delta_{\text{OUT}}I_2 + \delta_{\text{IN}}I_{\text{OTHER}2}$
-   $\Delta R_2 = \text{vacc(t)}(S_1 + E_1 + R_1 + S_2 + E_2) + \gamma I_2 - b \omega R_2 - \delta_{\text{OUT}}R_2 + \delta_{\text{IN}}R_{\text{OTHER}2}$
-   $\Delta D_2 = \alpha_2 \gamma I_2$
