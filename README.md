COMPOSABLE EVENT SOURCING WITH MONADS
=====================================

List of files
-------------

- [`slides.pdf`](https://raw.githubusercontent.com/dohzya/scalaio-2017-esmonad/master/slides.pdf): The slides
- `models`: The base model
- `V0`: (part **1.1**) Non-ES version
- `V1`: Adding ES - without creation
- `V2`: First EventHandler version
- `V2_2`: (part **1.2**) EventHandler as a function
- `V2_3`: EventHandler factory - final EventHandler version
- `V3`: ES test: making commands return both Event and State
- `V3_2`: (part **1.3**) ES version - commands return only event
- `V3_3`: Aggregating events in a seq
- `V4`: (part **3.2**) Sourced class - which is a writer monad
- `V4_2`: (part **3.2**) Sourced definition using `WriterT`
- `V5`: (part **3.4**) Splitting creation and updates
- `V5_2`: split definition using `Kleisli`
- `V6`: Sourced definition using `ReaderWriterState`
- `V6_2`: Split definition using `ReaderWriterState`

LICENSE
-------

Copyright (c) 2017 Fabernovel Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
