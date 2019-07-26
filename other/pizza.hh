// Autorzy:
// Damian Bodnar
// Piotr Zalas

#ifndef _PIZZA_HH_
#define _PIZZA_HH_

#include <array>
#include <type_traits>

template<typename... Kinds> 
struct Pizzeria
{
        // Struktury służące do rekurencyjnego zliczania 
        // wystąpień typu K na liście (T, Args...).
        template<typename K, typename... Args>
        struct sumuj 
        {
                static constexpr size_t suma = 0;
        };

        template<typename K, typename T, typename... Args>
        struct sumuj<K, T, Args...>
        {
                static constexpr size_t suma = ((std::is_same<T, K>::value) ? 1 : 0)
                                               + sumuj<K, Args...>::suma;
        };

        // Struktura mówiąca na którym miejscu na liście Kinds występuje element.
        // Liczymy od 0. Milcząco zakłada że argument występuje.
        template<typename K, typename... Args>
        struct finder
        {
                static constexpr size_t suma = 0;
        };

        template<typename K, typename T, typename... Args>
        struct finder<K, T, Args...>
        {
                static constexpr size_t suma = ((std::is_same<T, K>::value) ? 
                                                0 : 1 + (finder<K, Args...>::suma));
        };

        // Struktura reprezentująca pizzę.
        // Argumenty: lista długości sizeof...(Kinds) mówiąca ile kawałków
        // poszczególnego typu znajduje się w pizzy
        template<size_t... Parts>
        struct pizza
        {
                // Pizzeria z której pochodzi pizza
                using creator = Pizzeria<Kinds...>;

                // Lista powinna być zgodna z kolejnością Kinds.
                static constexpr std::array<size_t, sizeof...(Kinds)> as_array()
                {
                        return std::array<size_t, sizeof...(Kinds)>( {{ Parts... }} );
                }

                // Zwraca liczbę kawałków typu Kind
                template<typename Kind> 
                static constexpr size_t count()
                {
                        const auto array = as_array();
                        return array[ finder<Kind, Kinds...>::suma ];
                }

                // Podwaja liczbę kawałków w pizzy
                using sliced_type = pizza< 2 * Parts... >;
        };

        // Tworzy pizzę w wybranym rodzaju
        template<typename Kind> 
        struct make_pizza
        {
                // Asercja sprawdzająca poprawność parametru yumminess
                // static_assert(Kind::yumminess(0) == 0, "Error: yumminess(0) != 0!");
                // Sprawdza, czy Kinds się nie powtarzają lub nie występują na liście.
                static_assert(sumuj<Kind, Kinds...>::suma == 1, "Invalid Kind or Kinds list!");

                using type = pizza< (std::is_same<Kind, Kinds>::value ? 8 : 0)...  >;
        };

        // Pomocnicza struktura do wyznaczania mixu.
        // Umieszczona w tym miejscu, bo potrzebuje dostępu do Kinds...
        template<typename Kind1, typename Kind2>
        struct make_pizza_mix
        {
                // Wyznacza argument, dla którego K::yumminess ma największą
                // wartość na przedziale [0, sum].
                // W przypadku niejednoznaczności wybiera największy argument.
                template<typename K>
                static constexpr size_t max_value() 
                {
                        static_assert(K::yumminess(0) == 0, "Error: yumminess(0) != 0!");
                        size_t sum = Kind1::template count<K>()
                                     + Kind2::template count<K>();
                        size_t best = 0; // argument
                        auto best_val = K::yumminess(0); // wartość
                        for (size_t i = 1; i <= sum; i++) {
                                auto actual = K::yumminess(i);
                                if (actual >= best_val) {
                                        best_val = actual;
                                        best = i;
                                }
                        }
                        return best;
                }

                using type = pizza<(max_value<Kinds>())...>;
        };
};

template<typename Pizza1, typename Pizza2>
struct best_mix
{
        // Można mieszać tylko pizze z tego samego lokalu.
        static_assert(std::is_same<typename Pizza1::creator,
                                   typename Pizza2::creator>::value, 
                      "Invalid pizza mix!");
        
        using type = typename Pizza1::creator::
                     template make_pizza_mix<Pizza1, Pizza2>::type;
};

#endif
