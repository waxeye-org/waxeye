// A type-safe immutable homogenous "persistent" cons list.
// A cons list is either empty or a cons pair of T and a cons list.
export type ConsList<T> = Cons<T>|Empty;

// Creates a non-empty cons list.
export function cons<T>(head: T, tail: ConsList<T>): Cons<T> {
  return new Cons<T>(head, tail);
}

// Returns the empty cons list.
export function empty(): Empty {
  return Empty.instance;
}

// Methods implemented by both Empty and Cons<T>.
export interface ConsListInterface<T> extends Iterable<T> {
  // Check whether the list is an Empty.
  //
  // Using this method is preferable to the expensive and less readable
  // `instanceof Empty` check.
  isEmpty(): this is Empty;

  // Equivalent to [...list] but slightly faster.
  toArray(): T[];
}

// A non-empty cons list.
export class Cons<T> implements ConsListInterface<T> {
  // Internal, not public.
  // TODO(glebm): Add the internal modifier once it's implemented in TypeScript
  // https://github.com/Microsoft/TypeScript/issues/5228
  constructor(public readonly head: T, public readonly tail: ConsList<T>) {}

  public isEmpty(): this is Empty {
    return false;
  }

  public toArray(): T[] {
    const result: T[] = [this.head];
    let currentTail = this.tail;
    while (!currentTail.isEmpty()) {
      result.push(currentTail.head);
      currentTail = currentTail.tail;
    }
    return result;
  }

  public[Symbol.iterator](): Iterator<T> {
    let current: ConsList<T> = this;
    return {
      next(): IteratorResult<T> {
        if (current.isEmpty()) {
          // This typecast is necessary because TypeScript incorrectly
          // specifies both `done` and `value` as required.
          return {done: true} as IteratorResult<T>;
        }
        const value = current.head;
        current = current.tail;
        return {value} as IteratorResult<T>;
      },
    };
  }
}

// An empty cons list.
export class Empty implements ConsListInterface<never> {
  public static readonly instance = new Empty();
  private constructor() {}
  public isEmpty(): this is Empty {
    return true;
  }
  public toArray(): [] {
    return [];
  }
  public[Symbol.iterator](): Iterator<never> {
    return {
      next(): IteratorResult<never> {
        return {done: true} as IteratorResult<never>;
      },
    };
  }
}
