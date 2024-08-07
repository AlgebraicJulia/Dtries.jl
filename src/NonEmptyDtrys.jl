module NonEmptyDtrys

using ..SortedMaps
using ..Dtrys: AbstractPath, Path, head

using Test: AbstractTestSet
using MLStyle
using AssociatedTests, Test
import AbstractTrees
using Base: tail

struct LeafStorage{A}
    value::Ref{A}
end

struct NodeStorage{X}
    branches::SortedMap{Symbol,X}
end

struct NED{A}
    content::Union{LeafStorage{A},NodeStorage{NED{A}}}
end

content(t::NED) = getfield(t, :content)

struct Leaf{A} end

Leaf{A}(x::A) where {A} = NED{A}(LeafStorage{A}(Ref{A}(x)))
Leaf(x::A) where {A} = NED{A}(LeafStorage{A}(Ref{A}(x)))

@active Leaf(t) begin
    c = content(t)
    if c isa LeafStorage
        Some(c.value)
    else
        nothing
    end
end

@tests Leaf begin
    @test typeof(Leaf(1)) == NED{Int}
    @test typeof(Leaf{Real}(1)) == NED{Real}
end

struct Node{A} end

function Node{A}(m::SortedMap{Symbol,NED{A}}) where {A}
    NED{A}(NodeStorage{NED{A}}(m))
end

function Node(m::SortedMap{Symbol,NED{A}}) where {A}
    Node{A}(m)
end

function Node(pair::Pair{Symbol,NED{A}}, pairs...) where {A}
    Node(SortedMap{Symbol,NED{A}}(pair, pairs...))
end

@active Node(t) begin
    c = content(t)
    if c isa NodeStorage
        Some(c.branches)
    else
        nothing
    end
end

@tests Node begin
    @test typeof(Node(:A => Leaf(2))) == NED{Int}
    @test typeof(Node(:A => Leaf{Real}(2))) == NED{Real}
end

@tests NED begin
    @test Node(:home => Leaf(1)) isa NED
    @test Node(:home => Leaf(1), :work => Leaf(2)) isa NED
    @test Node(
        :home => Leaf(1),
        :work => Leaf(2),
        :library =>
            Node(:coffee => Leaf(3), :donut => Leaf(4))
    ) isa NED
end

function Base.:(==)(d1::NED, d2::NED)
    @match (d1, d2) begin
        (Leaf(x1), Leaf(x2)) => x1[] == x2[]
        (Node(b1), Node(b2)) => b1 == b2
        _ => false
    end
end

function AbstractTrees.children(t::NED)
    @match t begin
        Leaf(_) => ()
        Node(bs) => bs
    end
end

function AbstractTrees.printnode(io::IO, t::NED{A}; kw...) where {A}
    @match t begin
        Leaf(v) => print(io, v[])
        Node(bs) => print(io, "Dtry{", A, "}")
    end
end

function Base.show(io::IO, t::NED{A}) where {A}
    @match t begin
        Leaf(v) => print(io, "Leaf(", v[], ")")
        Node(bs) => print(io, "Node(", bs..., ")")
    end
end

function Base.show(io::IO, ::MIME"text/plain", t::NED{A}) where {A}
    AbstractTrees.print_tree(io, t; printkeys=true)
end

function Base.map(f, ::Type{B}, t::NED{A}) where {A,B}
    @match t begin
        Leaf(v) => Leaf{B}(f(v[]))
        Node(bs) =>
            Node{B}(SortedMap([n => map(f, B, t′) for (n, t′) in pairs(bs)], true))
    end
end

@tests map begin
    @test map(x -> x + 1, Int, Node(:home => Leaf(1))) == Node(:home => Leaf(2))
end

function filtermap(f, ::Type{B}, t::NED{A}) where {A,B}
    @match t begin
        Leaf(v) => @match f(v[]) begin
            Some(v′) => Some(Leaf{B}(v′))
            ::Nothing => nothing
        end
        Node(bs) => begin
            bs′ = Pair{Symbol,NED{B}}[]
            for (n, t) in pairs(bs)
                @match filtermap(f, B, t) begin
                    Some(t′) => push!(bs′, n => t′)
                    ::Nothing => nothing
                end
            end
            if !isempty(bs′)
                Some(Node{B}(SortedMap(bs′, true)))
            end
        end
    end
end

@tests filtermap begin
    t = Node(:a => Leaf(1), :b => Leaf(2))
    @test filtermap(x -> iseven(x) ? Some(x + 1) : nothing, Int, t).value == Node(:b => Leaf(3))
    @test filtermap(x -> isodd(x) ? Some(x + 1) : nothing, Int, t).value == Node(:a => Leaf(2))
end

function singleton(p::AbstractPath, v::A, B::Type=A) where {A}
    if isempty(p)
        Leaf{B}(v)
    else
        Node(head(p) => singleton(tail(p), v, B))
    end
end
export singleton

@tests singleton begin
    @test singleton(Path([]), 2) == Leaf(2)
    @test singleton(Path([]), 2, Real) == Leaf{Real}(2)
    @test singleton(Path([:a, :b]), 2) == Node(:a => Node(:b => Leaf(2)))
end

function lookup(t::NED, p::AbstractPath)
    @match t begin
        Leaf(x) =>
            if isempty(p)
                Some(x[])
            end
        Node(bs) =>
            if !isempty(p)
                @match bs[head(p)] begin
                    ::Nothing => nothing
                    Some(t′) => lookup(t′, tail(p))
                end
            end
    end
end
export lookup

@tests lookup begin
    t = Node(:home => Leaf(1))
    @test lookup(t, Path([:home])) == Some(1)
    @test isnothing(lookup(t, Path([])))
    @test isnothing(lookup(t, Path([:away])))
    @test isnothing(lookup(t, Path([:home, :base])))
    @test lookup(Leaf(2), Path([])) == Some(2)
end

function setatpath!(t::NED{A}, v::A, p::AbstractPath) where {A}
    @match t begin
        Leaf(x) =>
            if isempty(p)
                x[] = v
            else
                error("violated prefix-free invariant for Dtrys")
            end
        Node(bs) =>
            if isempty(p)
                error("violated prefix-free invariant for Dtrys")
            else
                @match bs[head(p)] begin
                    ::Nothing => (bs[head(p)] = singleton(tail(p), v))
                    Some(t′) => setatpath!(t′, v, tail(p))
                end
            end
    end
end
export setatpath!

@tests setatpath! begin
    t = Leaf(2)
    setatpath!(t, 1, Path([]))
    @test lookup(t, Path([])) == Some(1)
    t = Node(:a => Leaf(1))
    setatpath!(t, 2, Path([:b, :a]))
    @test lookup(t, Path([:b, :a])) == Some(2)
    @test_throws Exception setatpath!(t, 2, Path([:a, :b]))
end

"""
zipwith(f, t1::NED{A}, t2::NED{B}) where {A,B}

Produces a new trie whose Leaf Node at a path `p` is given by `f(t1[p], t2[p])`.

Throws an error if `t1` and `t2` are not of the same shape: i.e. they don't
have the exact same set of paths.
"""
function zipwith(f, t1::NED{A}, t2::NED{B}) where {A,B}
    @match (t1, t2) begin
        (Leaf(v1), Leaf(v2)) => Leaf(f(v1[], v2[]))
        (Node(bs1), Node(bs2)) => begin
            keys(bs1) == keys(bs2) || error("Cannot zip two directories of dissimilar keys. Returned:
                left tree: $(keys(bs1))
                right tree: $(keys(bs2))
            ")
            Node(
                SortedMap(
                    [n => zipwith(f, s1, s2) for ((n, s1), (_, s2)) in zip(pairs(bs1), pairs(bs2))]
                )
            )
        end
    end
end

@tests zipwith begin
    t1 = Node(:a => Leaf(1))
    t2 = Node(:a => Leaf(2))
    @test zipwith((x, y) -> x + y, t1, t2) == Node(:a => Leaf(3))
    t2′ = Node(:b => Leaf(2))
    @test_throws Exception zipwith((x, y) -> x + y, t1, t2′)
end

function Base.first(t::NED{A}) where {A}
    @match t begin
        Leaf(v) => v[]
        Node(bs) => first(bs.contents)
    end
end

function traverse(f, t, p=Path([]))
    @match t begin
        Leaf(v) => f(p, v[])
        Node(bs) => begin
            for (n, t) in pairs(bs)
                traverse(f, t, Path([p.segments; n]))
            end
        end
    end
end

"""
Given `path`, assuming `v1` is a Leaf and `v2` is a Node, we expect

Node(SortedPath[(k1=>v1), (k1=>v2) ...) => [([path..., k1] => v1, pairs([path..., k2], v2)]
"""
function Base.pairs(t::NED{A})::Vector{Pair{Path, A}} where {A}
    ps = Pair{Path, A}[]
    traverse((p, v) -> push!(ps, p => v), t)
    ps
end

@tests pairs begin
    t1 = Node(:a => Leaf(1))
    @test pairs(t1) == [Path([:a]) => 1]
    t2 = Node(:b => Node(:c => Leaf(3), :b => Leaf(2)), :a => Leaf(1))
    @test pairs(t2) == [Path([:a]) => 1, Path([:b, :b]) => 2, Path([:b, :c]) => 3]
end

# """
#     fold(emptycase::A, Leafcase, Nodecase, t::NED{A})::A

# Fold over `t` to produce a single value.

# Args:
# - `emptycase::A`
# - `Leafcase::eltype(t) -> A`
# - `Nodecase::OrderedDict{Symbol, A} -> A`
# """
# function fold(emptycase::A, Leafcase, Nodecase, t::NED{A}) where {A}
#     @match t begin
#         Leaf(v) => Leafcase(v[])
#         Node(bs) => Nodecase(
#             Pair{Symbol,NED{A}}(
#                 map(bs) do (n, t′)
#                     n => fold(emptycase, Leafcase, Nodecase, t′)
#                 end...
#             )
#         )
#     end
# end
# TODO: Nodecase does not take OrderedDict!

# for tab completion?
# function Base.propertynames(t::NED{A}) where {A}
#     @match t begin
#         Node(bs) => keys(bs)
#         _ => Symbol[]
#     end
# end

Base.keys(t) = Base.propertynames(t)

function flatten(t::NED{A}) where {A}
    @match t begin
        Leaf(v) => v[]
        Node(bs) => Node(n => flatten(v) for (n, v) in bs)
    end
end

end
