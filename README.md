This project is licensed under BSD-3-Clause (see LICENSE).

Use:
1. Instrument the program you want to debug (the debuggee) with GHC.Debug.Stub
2. Run the debuggee, note the socket number output in the terminal
3. Run ghc-debug-web
4. Visit localhost:3000 on your web browser
5. Select the appropriate socket number from the list, and press Connect to see debug info
