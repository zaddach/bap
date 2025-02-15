open Core_kernel.Std

(** Future library.

    {1 Overview}

    The purpose of the library is to provide mechanisms for reasoning
    about state based dynamic system. Due to a separation of concerns,
    the library allows to reason about such system purely
    mathematically, without any dependency on the actual
    representation of the state, or how the dynamism is
    handled. Putting it more simple, the library allows to reason
    about mathematical objects, whose value changes with time.  Such
    objects, are usually model some complex systems with a hidden
    state, that can be only observed. This kind of systems is hard and
    practically impossible to describe using inductive types. We reify
    such systems with coinduction. The library defines two main
    coinductive types: [future] and [stream]. The [future] type is
    dual of the option type (a co-option), and [stream] is a dual of
    the list (co-list).

    A value of type [future] is an object, with some observable state,
    that is not yet defined. We know, that it might happen, but it is not
    guaranteed. The [future] can be defined only once. Once it is
    defined, it cannot be changed. Basically, the time line of the
    [future] object is separated into two phases: on a first phase the
    value of the object is undefined, and on the second it is defined
    and fixed. Both phases can be empty, i.e., an object can be
    brought into life with a defined value, and an undefined future,
    might be never defined.

    A value of type [stream] is an infinite sequence of finite
    values. More precisely, a stream can be viewed as an object, whose
    value varies in time.

    The library can be also seen as a common denominator between
    different async libraries and methods, e.g., lwt, async, threads,
    forthcoming effective programming in multicore OCaml, etc. For
    example, the [future] are quite similar to the [Lwt.t] in [Lwt]
    library and to the [Deferred.t] of the [Async] library. There are
    few differences, however. The [future] library tries to separate
    concerns, so unlike [Lwt] or [Async] libraries, future can't fail.
    In other words, if a computation that computes the [future] fails,
    that just means, that this particular future has never
    occurred. If a user wants to represent a future value, that can
    fail or succeed, that he is welcome to use sum types, e.g.,
    [('a,'b) Result.t future]. The same is true for the async library.

    {1 Notion of time}

    The future library handles time in a special way. The notion of
    physical time is replaced with the notion of order. We consider
    only the precedence of events. There is no notion of simultaneity
    built into the model of the library. Every event occurs in its own
    separate time slot, i.e., all events are serialized in the time.

    That is not to say, that simultaneous events are not
    representable. The library just allows a user, to engineer its own
    timescale and define, what is simultaneous and what is not. For
    example, a clock timer can be represented as a stream of seconds,
    and everything that occurs after the start of the [n]'th second,
    but before the start of the [n+1]'th second, is simultaneous.


    {1 Main-loop}

    Since the internal state of the dynamic system is usually
    impossible to represent, it is modeled by a notion of primitive
    signals and promises. When a future is created a corresponding
    promise is made. The system, that models the dynamic system is
    responsible to fulfill the promise. A signal is akin to the
    promise, with only difference, that it can (and should) be
    fulfilled, or signaled, more than once.

    The use of promises and signals is totally under a user control
    and is separated from the rest of the library. They can be
    signaled from event loops, such as Lwt or Async main loops, or from
    a window system event loop. The only requirement, is that this
    calls should be serialized, if it is possible that they are made
    from different threads.

    A common way to bind [Lwt] thread with [Future] is to use
    [on_success] function (or [upon] function for [Async]'s [Deferred]):

    {[
      let future_of_thread t =
        let future,promise = Future.create () in
        Lwt.on_success t (Promise.fulfill promise);
        future
    ]}
*)
module Std : sig
  type 'a future
  type 'a promise
  type 'a stream
  type 'a signal


  (** Future is an object whose value will be decided somewhere in the
      future, if that future has occurred.

      Futures can be seen as memory cells that can be set only once,
      thus having two states: empty and filled.

      A future may occur at some point of physical time. A future
      is total. If a promise cannot be fulfilled due to an error,
      that just means, that in this world this future is not
      possible. Thus the future object models nonlinear tree-like
      time. To represent a computation, that has different futures,
      one can use either a sum type as a future value, or a tuple of
      futures. The former is preferred, if different future is
      decidable (i.e., only one path is possible). The latter is
      preferred if different variants are possible.

      A future is a monad, and most of the code should work with the
      future via the monadic interface, e.g.,

      {[
        let first_insn mem pc : mem Or_error.t future =
          Future.(Stream.nth pc 0 >>= fun fst ->
                  Stream.nth pc 1 >>= fun snd ->
                  return (Memory.range mem fst snd))
      ]}

      Note: the future is a common denominator between lwt thread,
      async deferred, native ocaml event, or any other value, that is
      defined asynchronously. Once can also think of futures and
      threads as a software pattern to work with callbacks. *)
  module Future : sig
    type 'a t = 'a future
    include Monad.S with type 'a t := 'a t
    include Applicative.S with type 'a t := 'a t

    module Args : Applicative.Args with type 'a arg := 'a t

    (** [create ()] creates a new future. The function returns a pair
        of the future itself and a promise that can be used to fulfill
        the future. *)
    val create : unit -> 'a t * 'a promise

    (** [upon f action] will call [action] as soon a future [f] occurs. *)
    val upon : 'a t -> ('a -> unit) -> unit

    (** [is_decided f] is true if a future [f] is already decided.  *)
    val is_decided : 'a t -> bool

    (** [peek f] will return [Some value] if future [f] has already
        occurred with this [value].  *)
    val peek : 'a t -> 'a option

    (** [peek_exn f]
        will evaluate to [x] iff [is_decided f && peek f x = Some x] *)
    val peek_exn : 'a t -> 'a
  end

  (** An promise to provide a value in a future.*)
  module Promise : sig
    type 'a t = 'a promise

    (** [fulfill promise] will fill a future value associated with a
        [promise] and evaluate to [unit] if promise is not yet
        fulfilled.

        All actions associated with the future will be called in the
        context of the function calling [fulfull].

        The [fulfill] function is not thread-safe, in the
        sense, that [fulfill] calls to the same object made from different
        threads should be serialized. Note: since [fulfill] should be
        called once, it is rarely a case. *)
    val fulfill : 'a t -> 'a -> unit

    (** [is_fulfilled promise] is [true] if associated promise is
        already fulfilled *)
    val is_fulfilled : 'a t -> bool
  end

  (** A stream of elements.

      A stream is an infinite sequence of elements. Since the stream
      is defined coinductively it can be processed only
      corecursively. That means that in general, one cannot aggregate
      a stream into normal (inductive) data. But it is still possible
      to get an aggregate snapshot of some intermediate state in the
      form of futures.

      Streams can be observed and combined. There is no built in
      notion of the end of stream and all streams are considered
      infinite. It is still possible to simulate an end of stream, by
      using futures, that designates the end of stream condition.

      Streams can be made lazy in the sense that if no one is watching
      a stream, then no work should be performed to feed the
      stream. This requires some cooperation from the feeder, as it
      should use [on_subscribe] and [on_unsubscribe] functions, to
      react on user's subsriptions. The [has_subscribers] is also
      useful.

      Streams also provide some mechanism for a pushback, that allows
      a cooperative sink to limit his rate. The pushback interface
      consists of two functions:

      - [wait] that should be called by a consumer, when it wants to ask
      a producer to wait for a moment;

      - [on_wait] that is called when any consumer requested for a
      pause.

      The pushback interface is not mandatory.
  *)
  module Stream : sig
    type 'a t = 'a stream
    type id

    (** [create ()] retuns a stream and a signal handler that is used
        to feed the stream. Every time a value is signaled, it will
        occur in the stream. *)
    val create : unit -> 'a t * 'a signal

    (** [from f] returns a stream that is generated from successive
        applications of a function [f]. A new value is produced by a
        stream, every time it is signaled with associated signal
        handler. *)
    val from : (unit -> 'a) -> 'a t * unit signal

    (** [unfold ~init ~f] a more general than [from] way of building a
        stream, that allows to pass state between consecutive
        invocations of the generator function.  A new value is
        produced by a stream, every time it is signaled with an
        associated signal handler. *)
    val unfold : init:'b -> f:('b -> ('a * 'b)) -> 'a t * unit signal

    (** [unfold_until ~init ~f] returns [(stream,signal,future)] is the
        same as [unfold], except that function [f] is called until it
        returns a [None] value. Once this happens, the [future]
        becomes determined. *)
    val unfold_until : init:'b -> f:('b -> ('a * 'b) option) ->
      'a t * unit signal * unit future

    (** [unfold' ~init ~f] is a batched version of the [unfold]
        function. A new value is produced by a stream, every time it
        is signaled with associated signal handler. *)
    val unfold' : init:'b -> f:('b -> ('a Queue.t * 'b)) -> 'a t * unit signal

    (** [repeat x] returns a stream [xs] and a signal [s]. Every time
        [s] is signaled stream [xs] will produce a value [x] *)
    val repeat : 'a -> 'a t * unit signal

    (** [of_list xs] returns a stream [ss], a signal [s] and a future
        [es]. Stream will produce consequently elements of [xs] every
        time the signal [s] is sent. Once all elements are produced
        the future [es] will occur signifying the end of the
        underlying sequence. All consecutive signals from [es] are
        ignored. *)
    val of_list : 'a list -> 'a t * unit signal * unit future

    (** [of_array xs] returns a stream [ss], a signal [s] and a future
        [es]. Stream will produce consequently elements of [xs] every
        time the signal [s] is sent. Once all elements are produced
        the future [es] will occur signifying the end of the
        underlying sequence. All consecutive signals from [es] are
        ignored. *)
    val of_array : 'a array -> 'a t * unit signal * unit future

    (** [of_seq xs] returns a stream [ss], a signal [s] and a future
        [es]. Stream will produce consequently elements of [xs] every
        time the signal [s] is sent. Once all elements are produced
        the future [es] will occur signifying the end of the
        underlying sequence. All consecutive signals from [es] are
        ignored.*)
    val of_sequence : 'a Sequence.t -> 'a t * unit signal * unit future


    (** {2 Subscriber interface}

        In order to start to monitor a stream, a user should subscribe
        to the stream using one of the subscription functions:
        [watch], [observe], [subscribe].

        The subscription can be canceled by using an [unsubscribe]
        function, or by throwing an exception from the callback
        function. The latter plays well with `with_return` function.*)

    (** [watch s f] watches a stream [s] with a function [f].  A
        subscription identifier is passed to the function, so it can
        be used to unsubscribe from the stream directly from the
        function.*)
    val watch : 'a t -> (id -> 'a -> unit) -> unit

    (** [observe s f] is like [watch] but a subscription identifier is
        not passed to the function [f]. *)
    val observe : 'a t -> ('a -> unit) -> unit

    (** [subscribe s f] subscribe to a stream [s] with a function
        [f]. A subscription identifier is returned. *)
    val subscribe : 'a t -> ('a -> unit) -> id

    (** [unsubscribe s id] stop calling a function that was has a
        provided identifier [id] *)
    val unsubscribe : 'a t -> id -> unit

    (** [wait xs] a polite way to notify a producer to slow down.
        Note: producer is not required to obey.   *)
    val wait : 'a t -> unit

    (** {2 Publisher interface}  *)

    (** [has_subscribers s] is true if someone is watching for the stream  *)
    val has_subscribers : 'a t -> bool

    (** [on_subscribe s f] will call a function [f] every time someone
        is subscribed to a stream [s] *)
    val on_subscribe : 'a t -> (id -> unit) -> unit

    (** [on_unsubscribe s f] will call a function [f] every time
        someone has canceled subscription to a stream [s] *)
    val on_unsubscribe : 'a t -> (id -> unit) -> unit

    (** [on_wait s f] will be called every time someone, watching a
        stream [s], will call [wait s] to ask a producer to slow down.*)
    val on_wait : 'a t -> (unit -> unit) -> unit

    (** {2 Combinators} *)


    (** [s' = map' s ~f] apply function [f] for each value of a
        stream [s] and push values from a resulting queue into the
        stream [s'].
        Example:
        {[
          let q,p = of_list ['a','b','c', '.']
          let q' = map q ~f:(function
              | 'a'..'z' as c ->
                Queue.of_list Char.[uppercase c; lowercase c]
              | c -> Queue.singleton c
        ]}

        Will produce:

        [<A; a; B; b; C; c; .>]
    *)
    val map' : 'a t -> f:('a -> 'b Queue.t) -> 'b t

    (** [map ss ~f] returns new stream, that is build by application
        of a function [f] to each element of the stream [ss] *)
    val map : 'a t -> f:('a -> 'b) -> 'b t

    (** [filter_map s ~f] for each value [x] in stream [s], produce
        [y] if [f x] is [Some y], otherwise ignore [x] *)
    val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

    (** [filter s f] produce a stream that contains the elements of
        stream [s], for which [f] evaluates to true.  *)
    val filter : 'a t -> f:('a -> bool) -> 'a t


    (** [merge xs ys f] merges streams [xs] and [ys] using function
        [f].  *)
    val merge : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

    val apply : ('a -> 'b) t -> 'a t -> 'b t

    val split : 'a t -> f:('a -> 'b * 'c) -> 'b t * 'c t

    val zip : 'a t -> 'b t -> ('a * 'b) t

    val unzip : ('a * 'b) t -> 'a t * 'b t

    val once : 'a t -> 'a t

    (** [parse ss ~init ~f] parses stream [ss] and builds new stream
        [ss']. Function [f] is applied to each consecutive element of
        the list [ss] with a state [s]. If function [f] returns
        [None,s'], then no value is produced in the output state and
        state [s'] is passed to the next invocation of function
        [f]. If it returns [Some x, s'], then value [x] is produced by
        the output stream and state [s'] is passed to a consecutive
        invocation of [f]. If it state type ['b] is an instance of a
        list type, then parse will be a push down automaton. With
        arbitrary type of state it is possible to build automatons
        that falls between PDA and Turing Machine (not including the
        latter). *)
    val parse : 'a t -> init:'b -> f:('b -> 'a -> 'c option * 'b) -> 'c t

    (** [foldw ss n ~init ~f] performs a windowed fold of the stream.
        A function [f] is folded over [n] consecutive elements of [ss],
        then the result is produced into the output stream, the window
        is shifted by [stride] (defaults to one) and function [f]
        applied to the next [n] elements. For example, if stream [ss]
        produced the following sequence of elements:

        {[1,2,3,4,5,6,7,8]}

        and windows length [n] is equal to [3], then the function [f]
        will be applied to a sequences:
        {[[1,2,3], [2,3,4], [3,4,5], [4,5,6], [5,6,7], [6,7,8]]}.

        Example, a moving average filter implemented with [foldw]:

        {[
          let moving_average ss n =
            Float.(foldw ss n ~init:zero ~f:(+) >>| fun s / of_int n)

        ]} *)
    val foldw : ?stride:int -> 'a t -> int -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

    (** [frame ~clk s ~init ~f] will gather elements of [s] into frames, 
        where the start of the new frame is signaled by a stream [clk]. 
        The function is very similar to [foldw] except, that the window 
        is determined dynamically by a [clk] stream. This function is
        useful to build custom time scales.

        The semantics of the function can be clarified with the
        following description:
        1. Every time a stream [s] produces a value it is buffered
        2. Every time a stream [clk] produces a value, a function [f]
           is folded over all buffered value, and the result is put
           into the output stream. The internal buffer is cleared
           afterwards.


        Example
        -------

        Consider the following timing diagram, where each row
        represents a stream, and columns represent time. Elements of
        the [clk] stream are depicted with a [T] symbol.

        {v
          clk:    T         T        T  T      T     T
           ss: 123 56 123 12  1234 4      1234  1
        v}

        will be framed in the following way:

        [[123], [5612312], [12344], [], [1234], [1]]

        Note: since all streams should be serialized it is impossible,
        that two events occur at the same time. So at the same column
        of the timing diagram there can be only one event.  *)
    val frame : clk:unit t -> 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

    (** [sample ~clk ss] is semantically the same as
        [frame ~clk ss >>| fst] *)
    val sample : clk:unit t -> 'a t -> 'a option t

    (** [hd s] returns a [future] that will occur as soon, as stream
        [s] will produce a value.
        Note: if [hd] is called on a stream, that already produced
        some values, the returned future will still be fulfilled on
        the first value, that will be put into the stream after the
        future is created.
    *)
    val hd : 'a t -> 'a future


    (** [tl s] ignores the next occurrence in the stream [s]  *)
    val tl : 'a t -> 'a t

    (** [find xs f] returns a future that will be fulfilled with a
        first value for which a function [f] is [true]. *)
    val find : 'a t -> f:('a -> bool) -> 'a future

    (** [find_map xs f] returns a future that will be fulfilled by a
        result of a first invocation of [f] to an element of the stream,
        that evaluated to [Some] value *)
    val find_map : 'a t -> f:('a -> 'b option) -> 'b future


    (** [take xs n] returns a future that will evaluate to [n] values
        of the stream [xs] that has occurred after the future was
        created.  *)
    val take : 'a t -> int -> 'a list future

    (** [nth xs n] returns [n]'th element of the stream [xs]. The
        element is [n]'th with respect to the future [f], if was [n]'th
        element of the stream after the creation of the stream.  *)
    val nth  : 'a t -> int -> 'a future

    (** [upon e xs] returns a future that will be fulfilled with a
        last value of a stream [xs] before an event [e] has
        occurred. If at the time when the event [e] occurs, the stream
        [xs] didn't produce any elements, then the future will not be
        fulfilled.  *)
    val upon : unit future -> 'a t -> 'a future

    (** [before e xs] returns a list that contains elements of 
        the stream [xs] that occurred before the event [e] *)
    val before: unit future -> 'a t -> 'a list future

    (** [last_before e xs n] returns a list of length up to [n], that
        contains last elements of the stream [xs] that occurred before the
        event [e] *)
    val last_before : unit future -> 'a t -> int -> 'a list future

  end


  (** A handler to produce elements in streams.  *)
  module Signal : sig
    type 'a t = 'a signal

    (** [send s x] sends value [x] to an associated stream.  *)
    val send  : 'a signal -> 'a -> unit

    (** [repeat s ~times:n x] sends value [x] to an associated
        stream [n] times *)
    val repeat: 'a signal -> times:int -> 'a -> unit
  end
end
