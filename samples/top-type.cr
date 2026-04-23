@[Slot]
def foo(x)
  1
end

@[Slot]
def foo(x : _)
  2
end

@[Slot]
def foo(x : T) forall T
  3
end

pp foo(1)
