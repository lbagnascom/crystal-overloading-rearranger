module M
end

class C
  include M
end

class T < C
end

@[Slot]
def f(x : C)
	"Class C"
end

@[Slot]
def f(x : M)
	"Module"
end

pp f(T.new)
