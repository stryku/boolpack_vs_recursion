#include <type_traits>
#include <utility>

namespace proposal 
{

    namespace fold
    {
        template <typename... Tn> 
        struct disjunction : std::integral_constant<bool, (false || ... || bool(Tn::value))> {};

        template <typename... Tn> 
        struct conjunction : std::integral_constant<bool, (true && ... && bool(Tn::value))> {};
    }

    namespace bools
    {
        template <bool... Bn>
        struct Bools {};

        template <typename... Tn> 
        struct disjunction : std::negation<std::is_same<Bools<bool(Tn::value)...>, Bools<(Tn::value && false)...>>> {};

        template <typename... Tn> 
        struct conjunction : std::is_same<Bools<bool(Tn::value)...>, Bools<(Tn::value || true)...>> {};
    }

    template<typename... Tn>
    using disjunction = PROPOSAL_VERSION::disjunction<Tn...>;

    template<typename... Tn>
    using conjunction = PROPOSAL_VERSION::conjunction<Tn...>;
}

namespace best_case
{
    template <typename S>
    struct make_case;

    template <std::size_t ...Tn>
    struct make_case<std::integer_sequence<std::size_t, 0, Tn...>>
    {
        using type = std::integer_sequence<std::size_t, 0, Tn...>;
    };
}

namespace worse_case
{
    template <typename S>
    struct make_case;

    template <std::size_t ...Tn>
    struct make_case<std::integer_sequence<std::size_t, 0, Tn...>>
    {
        using type = std::integer_sequence<std::size_t, Tn..., 0>;
    };
}

namespace light
{
    template <std::size_t N>
    using Type = std::integral_constant<std::size_t, N>;
}


namespace heavy
{
    template <std::size_t N, std::size_t count, std::size_t... Vn> 
    struct HeavyType;

    template <std::size_t N, std::size_t... Vn> 
    struct HeavyType<N, 0, Vn...> 
    { 
        static constexpr auto value = N; 
    };

    template <std::size_t N, std::size_t count, std::size_t... Vn>
    struct HeavyType
    {
        static constexpr auto value = HeavyType<N, count - 1, 1, Vn...>::value;
    };

    template <std::size_t N>
    using Type = HeavyType<N, 50>;
}


template <typename... Tn>
using IsZeroIn = std::negation<NAMESPACE::conjunction<Tn...>>;

template <template <typename...> class S, typename H> struct Find_;
template <template <typename...> class S, std::size_t... Hs> struct Find_<S, std::integer_sequence<std::size_t, Hs...>> { using type = S<TO_FIND_TYPE_NAMESPACE::Type<Hs>...>; };
template <template <typename...> class S, typename H> using Find = typename Find_<S, H>::type;
using Sequence = typename CASE::make_case<std::make_index_sequence<1024>>::type;

static constexpr bool Found = Find<IsZeroIn, Sequence>::value;
