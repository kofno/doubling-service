This extends the Doubling Service from Parallel and Concurrent Programming in
Haskell.

The service itself is basic. The point of the exercise is to provide more
robust server features, like you might expect in a production server.

The book recommends the following exercises:

  * Handle Ctrl+C interrupts gracefully
  * Handle parse failures when talk recieves a value that's not a number
  * Handle when client cuts connection unexpectly (or network failure)
  * Should there be an upper bound on the number of client connections?
  * ~~Logging activity to a file (rather then the console)~~
