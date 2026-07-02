module M
end

class C
end

class T < C
	include M
end

@[Slot]
def f(x : C)
	"Class"
end

@[Slot]
def f(x : M)
	"Module"
end

pp f(T.new)
