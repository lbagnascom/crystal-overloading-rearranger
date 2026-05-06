@[Slot]
def foo(x)
  "x"
end

@[Slot]
def foo(x : _)
  "x : _"
end

@[Slot]
def foo(x : T) forall T
  "x : T"
end

pp foo(true)
