using .NonEmptyDtries: NED
using .NonEmptySortedMaps
using MLStyle
import AbstractTrees

mutable struct Dtry{A}
    content::Union{NED{A},Nothing}
end
export Dtry

content(t::Dtry) = getfield(t, :content)

struct Empty{A}
    function Empty{A}() where {A}
        Dtry{A}(nothing)
    end
end
export Empty

@active Empty(t) begin
    isnothing(content(t))
end

@tests Empty begin
    @test typeof(Empty{Int}()) == Dtry{Int}
    @test (@match Empty{Int}() begin
        Empty() => true
        _ => false
    end)
end

struct NonEmpty{A}
    function NonEmpty{A}(t::NED{A}) where {A}
        Dtry{A}(t)
    end

    function NonEmpty(t::NED{A}) where {A}
        NonEmpty{A}(t)
    end
end
export NonEmpty

@active NonEmpty(t) begin
    c = content(t)
    if !isnothing(c)
        Some(c)
    end
end

@tests NonEmpty begin
    @test typeof(NonEmpty(NonEmptyDtries.Leaf(2))) == Dtry{Int}
    @test (@match NonEmpty(NonEmptyDtries.Leaf(2)) begin
        NonEmpty(_) => true
        _ => false
    end)
    @test (@match NonEmpty(NonEmptyDtries.Leaf(2)) begin
        NonEmpty(t) => t == NonEmptyDtries.Leaf(2)
        _ => false
    end)
end

struct Leaf{A}
    function Leaf{A}(x::A) where {A}
        Dtry{A}(NonEmptyDtries.Leaf{A}(x))
    end

    function Leaf(x::A) where {A}
        Leaf{A}(x)
    end
end
export Leaf

@active Leaf(t) begin
    @match content(t) begin
        NonEmptyDtries.Leaf(v) => Some(v)
        _ => nothing
    end
end

@tests Leaf begin
    @test content(Leaf(1)) == NonEmptyDtries.Leaf(1)
    @test (@match Leaf(1) begin
        Leaf(x) => x[]
        _ => nothing
    end) == 1
end

struct Node{A}
    function Node{A}(m::NESM{Symbol,NED{A}}) where {A}
        Dtry{A}(NonEmptyDtries.Node{A}(m))
    end

    function Node(m::NESM{Symbol,NED{A}}) where {A}
        Node{A}(m)
    end

    function Node{A}(m::NESM{Symbol,Dtry{A}}) where {A}
        nonempties = Pair{Symbol,NED{A}}[]
        for (n, d) in pairs(m)
            @match d begin
                NonEmpty(t) => push!(nonempties, n => t)
                Empty() => nothing
            end
        end
        Dtry{A}(NonEmptyDtries.Node{A}(NESM{Symbol,NED{A}}(nonempties, true)))
    end

    function Node(m::NESM{Symbol,Dtry{A}}) where {A}
        Node{A}(m)
    end

    function Node{A}(pairs...) where {A}
        if isempty(pairs)
            Empty{A}()
        else
            Node{A}(NESM(pairs...))
        end
    end

    function Node(pair::Pair{Symbol,Dtry{A}}, pairs...) where {A}
        Node{A}(NESM(pair, pairs...))
    end
end
export Node

@tests Node begin
    @test Node(NESM(:a => NonEmptyDtries.Leaf(1))) isa Dtry{Int}
    @test Node(NESM(:a => Leaf(1))) isa Dtry{Int}
    @test content(Node(:a => Leaf(2))) == NonEmptyDtries.Node(NESM(:a => NonEmptyDtries.Leaf(2)))
end

@active Node(m) begin
    @match content(m) begin
        NonEmptyDtries.Node(m) => Some(m)
        _ => nothing
    end
end

function Base.:(==)(d1::Dtry{A}, d2::Dtry{A}) where {A}
    content(d1) == content(d2)
end

function AbstractTrees.children(d::Dtry)
    @match d begin
        Empty() => ()
        NonEmpty(t) => AbstractTrees.children(t)
    end
end

function AbstractTrees.printnode(io::IO, d::Dtry{A}; kw...) where {A}
    @match d begin
        Empty() => print(io, Empty{A})
        NonEmpty(t) => AbstractTrees.printnode(io, t; kw...)
    end
end

function Base.show(io::IO, d::Dtry)
    @match d begin
        Empty() => print(io, "Empty()")
        NonEmpty(t) => show(io, t)
    end
end

function Base.show(io::IO, ::MIME"text/plain", d::Dtry)
    AbstractTrees.print_tree(io, d; printkeys=true)
end

function Base.map(f, ::Type{B}, t::Dtry{A}) where {A,B}
    @match t begin
        NonEmpty(t) => NonEmpty{B}(map(f, B, t))
        Empty() => Empty{B}()
    end
end

@tests map begin
    t = Node(:a => Leaf(1))
    @test map(x -> x + 1, Int, t) == Node(:a => Leaf(2))
    @test map(x -> x + 1, Int, Empty{Int}()) == Empty{Int}()
end

function singleton(p::AbstractPath, x::A, B::Type=A) where {A}
    Dtry{B}(NonEmptyDtries.singleton(p, x, B))
end

@tests singleton begin
    @test singleton(Path([]), 2) == Leaf(2)
    @test singleton(Path([]), 2, Real) == Leaf{Real}(2)
    @test singleton(Path([:a, :b]), 2) == Node(:a => Node(:b => Leaf(2)))
end

function lookup(d::Dtry, p::AbstractPath)
    @match d begin
        Empty() => nothing
        NonEmpty(t) => NonEmptyDtries.lookup(t, p)
    end
end

@tests lookup begin
    d = Node(:home => Leaf(1))
    @test lookup(d, Path([:home])) == Some(1)
    @test isnothing(lookup(d, Path([])))
    @test lookup(Leaf(2), Path([])) == Some(2)
    @test isnothing(lookup(Empty{Int}(), Path([])))
end

function Base.getindex(d::Dtry, p::AbstractPath)
    @match lookup(d, p) begin
        Some(v) => v
        ::Nothing => throw(KeyError(p))
    end
end

function Base.getindex(d::Dtry, p::Vector{Symbol})
    getindex(d, Path(p))
end

function Base.getindex(d::Dtry, p::Symbol...)
    getindex(d, Path([p...]))
end

@tests getindex begin
    d = Node(:home => Leaf(1))
    @test d[Path([:home])] == 1
    @test d[[:home]] == 1
    @test d[:home] == 1
    d = Leaf(1)
    @test d[] == 1
end

function setatpath!(d::Dtry{A}, v::A, p::AbstractPath) where {A}
    @match d begin
        Empty() => (d.content = NonEmptyDtries.singleton(p, v, A))
        NonEmpty(t) => NonEmptyDtries.setatpath!(t, v, p)
    end
end

@tests setatpath! begin
    t = Leaf(2)
    setatpath!(t, 1, Path([]))
    @test lookup(t, Path([])) == Some(1)
    t = Node(:a => Leaf(1))
    setatpath!(t, 2, Path([:b, :a]))
    @test lookup(t, Path([:b, :a])) == Some(2)
    @test_throws Exception setatpath!(t, 2, Path([:a, :b]))
    t = Empty{Int}()
    setatpath!(t, 5, Path([:a, :b]))
    @test t == Node(:a => Node(:b => Leaf(5)))
    setatpath!(t, 1, Path([:a, :b]))
    @test t == Node(:a => Node(:b => Leaf(1)))
end

function Base.setindex!(d::Dtry{A}, x::A, p::AbstractPath) where {A}
    setatpath!(d, x, p)
end

function Base.setindex!(d::Dtry{A}, x::A, p::Vector{Symbol}) where {A}
    setatpath!(d, x, Path(p))
end

function Base.setindex!(d::Dtry{A}, x::A, path::Symbol...) where {A}
    setatpath!(d, x, Path([path...]))
end

@tests setindex! begin
    d = Empty{Int}()
    d[Path([:a, :b])] = 2
    @test d == Node(:a => Node(:b => Leaf(2)))
    d[[:a, :b]] = 1
    @test d == Node(:a => Node(:b => Leaf(1)))
    d[:a, :b] = 0
    @test d == Node(:a => Node(:b => Leaf(0)))
    d = Empty{Int}()
    d[] = 2
    @test d[] == 2
end

"""
returns Vector{Pair{Path, Value}}
"""
function Base.pairs(t::Dtry{A}) where {A}
    @match t begin
        Empty() => Pair{Path,A}[]
        NonEmpty(t) => pairs(t)
    end
end

@tests pairs begin
    @test pairs(Empty{Int}()) == Pair{Path,Int}[]
    t1 = Node(:a => Leaf(1))
    @test pairs(t1) == [Path([:a]) => 1]
    t2 = Node(:b => Node(:c => Leaf(3), :b => Leaf(2)), :a => Leaf(1))
    @test pairs(t2) == [Path([:a]) => 1, Path([:b, :b]) => 2, Path([:b, :c]) => 3]
end

function frompairs(pairs::Vector{Pair{Path,A}}, B::Type=A) where {A}
    d = Empty{B}()
    for (p, v) in pairs
        d[p] = v
    end
    d
end

function frompairs(pairs::Vector{Pair{Vector{Symbol},A}}, B::Type=A) where {A}
    d = Empty{B}()
    for (p, v) in pairs
        d[p] = v
    end
    d
end

@tests frompairs begin
    d = Node(:a => Leaf(1))
    @test frompairs(pairs(d)) == d
    d2 = Node(:b => Node(:c => Leaf(3), :b => Leaf(2)), :a => Leaf(1))
    @test frompairs(pairs(d2)) == d2
end

function Dtry{A}() where {A}
  Empty{A}()
end

function Dtry{A}(pairs::Vector) where {A}
    frompairs(pairs, A)
end

function Dtry(pairs::Vector)
    frompairs(pairs)
end

function Dtry{A}(pair::Pair, pairs...) where {A}
    frompairs([pair, pairs...], A)
end

function Dtry{A}() where {A}
    Empty{A}()
end

# We don't allow Dtry(), because that would have to be typed as Dtry{Any}, which
# would lead to pain and suffering
function Dtry(pair::Pair, pairs...)
    frompairs([pair, pairs...])
end

@tests Dtry begin
    @test Dtry{Int}() isa Dtry
    @test Dtry(Path([:a]) => 2) == Node(:a => Leaf(2))
    @test Dtry([:a] => 2) == Node(:a => Leaf(2))
    @test Dtry(Symbol[] => 2) == Leaf(2)
    @test_throws Exception Dtry([:a] => 2, [:a, :b] => 3)
    @test_throws MethodError Dtry()
    @test Dtry{Int}() == Empty{Int}()
end

function flatmap(f, ::Type{B}, d::Dtry{A})::Dtry{B} where {A,B}
    Dtry{B}(NonEmptyDtries.flatfiltermap(d -> content(f(d)), B, content(d)))
end

@tests flatmap begin
    d1 = flatmap(
        x -> iseven(x) ? Node(:x => Leaf(x)) : Empty{Int}(),
        Int,
        Node(:a => Leaf(1), :b => Leaf(2))
    )
    d2 = Node(:b => Node(:x => Leaf(2)))
    @test d1 == d2
end

function flatten(d::Dtry{Dtry{A}})::Dtry{A} where {A}
    flatmap(identity, A, d)
end

@tests flatten begin
    @test flatten(Node(:a => Leaf(Node(:b => Leaf(1))))) == Node(:a => Node(:b => Leaf(1)))
    d1 = flatten(Node(:a => Leaf(Empty{Int}()), :b => Leaf(Node(:c => Leaf(2)))))
    d2 = Node(:b => Node(:c => Leaf(2)))
    @test d1 == d2
end
