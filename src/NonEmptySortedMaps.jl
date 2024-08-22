module NonEmptySortedMaps
using Test
using AssociatedTests

struct NonEmptySortedMap{K,V}
    pairs::Vector{Pair{K,V}}
    function NonEmptySortedMap{K,V}(values::Vector{Pair{K,V}}, presorted=false) where {K,V}
        length(values) > 0 || error("must provide a non-empty vector of pairs")
        if presorted
            new(values)
        else
            new(sort(values, by=first))
        end
    end
    function NonEmptySortedMap(values::Vector{Pair{K,V}}, presorted=false) where {K,V}
        NonEmptySortedMap{K,V}(values, presorted)
    end
    function NonEmptySortedMap{K,V}(pair::Pair{K, V}, pairs...) where {K,V}
        NonEmptySortedMap{K,V}([pair, pairs...])
    end
    function NonEmptySortedMap(pair::Pair{K, V}, pairs...) where {K, V}
        NonEmptySortedMap{K,V}([pair, pairs...])
    end
end
export NonEmptySortedMap

const NESM = NonEmptySortedMap
export NESM

function Base.:(==)(m1::NESM{K,V}, m2::NESM{K,V}) where {K,V}
    m1.pairs == m2.pairs
end

function Base.getindex(m::NESM{K,V}, n::K)::Union{Some{V},Nothing} where {K,V}
    idx = searchsortedfirst(m.pairs, (n, nothing), by=first)
    if idx > length(m.pairs)
        return nothing
    end
    (n′, v) = m.pairs[idx]
    if n == n′
        Some(v)
    else
        nothing
    end
end

@tests getindex begin
    pairs = NESM(:foo => 1, :gar => 5)
    @test pairs[:foo] == Some(1)
    @test isnothing(pairs[:bar])
end

function Base.setindex!(m::NESM{K,V}, x::V, n::Symbol) where {K,V}
    idx = searchsortedfirst(m.pairs, (n, nothing), by=first)
    if idx > length(m.pairs)
        push!(m.pairs, n => x)
    elseif first(m.pairs[idx]) == n
        error("already contains the key $n")
    else
        insert!(m.pairs, idx, n => x)
    end
end

@tests setindex! begin
    m = NESM(:bar => 0, :foo => 1)
    m[:baz] = 2
    @test m[:baz] == Some(2)
    m[:zaz] = 1
    @test m[:zaz] == Some(1)
    @test_throws Exception m[:zaz] = 2
end

function Base.pairs(m::NESM)
    m.pairs
end

function Base.iterate(m::NESM)
    Base.iterate(m.pairs)
end

function Base.iterate(m::NESM, i)
    Base.iterate(m.pairs, i)
end

Base.keys(m::NESM) = first.(m.pairs)

end
