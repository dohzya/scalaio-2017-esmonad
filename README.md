COMPOSABLE EVENT SOURCING WITH MONADS
=====================================

List of files:

- `models`: The base model
- `V0`: (1.1) (non-ES version)
- `V1`: Adding ES - without creation
- `V2`: First EventHandler version
- `V2_2`: (1.2) EventHandler as a function
- `V2_3`: EventHandler factory - final EventHandler version
- `V3`: ES test: making commands return both Event and State
- `V3_2`: (1.3) ES version - commands return only event
- `V3_3`: Aggregating events in a seq
- `V4`: (3.2) Sourced class - which is a writer monad
- `V4_2`: (3.2) Sourced definition using `WriterT`
- `V5`: (3.4) Splitting creation and updates
- `V5_2`: split definition using `Kleisli`
- `V6`: Sourced definition using `ReaderWriterState`
- `V6_2`: Split definition using `ReaderWriterState`
