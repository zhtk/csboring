#ifndef _BST_IMP_H_
#define _BST_IMP_H_

#include "bst.h"
#include <algorithm>
#include <tuple>

template <typename T>
BST<T>::BST(std::initializer_list<T> list) : 
            BST(list.begin(), list.end()) {};

template <typename T>
template <typename Iter>
BST<T>::BST(Iter b, Iter e) : 
            BST(b == e ? BST<T>() : (BST<T>(b, e - 1) + *(e - 1))) {}

template <typename T>
BST<T>::BST(T value, BST left, BST right) : 
            m_root(std::make_shared<Node>(value, left.m_root, right.m_root)) {}

template <typename T>
BST<T> BST<T>::left() const
{
    if(empty())
        throw std::logic_error("BST<T>::left() - Empty tree");

    return BST<T>(m_root->m_left);
}

template <typename T>
BST<T> BST<T>::right() const
{
    if(empty())
        throw std::logic_error("BST<T>::left() - Empty tree");

    return BST<T>(m_root->m_right);
}

template <typename T>
T const & BST<T>::value() const
{
    if(empty())
        throw std::logic_error("BST<T>::value() - Empty tree");

    return m_root->m_value;
}

template <typename T>
bool BST<T>::empty() const
{
    return !m_root;
}

template <typename T>
T const & BST<T>::min() const
{
    return (!left().empty() ? left().min() : value());
}

template <typename T>
T const & BST<T>::max() const
{
    return (!right().empty() ? right().max() : value());
}

template <typename T>
template <typename Acc, typename Functor>
Acc BST<T>::fold(Acc a, Functor f) const
{
    if(empty())
        return a;
    
    return right().fold(f(left().fold(a, f), value()), f);
}

template <typename T>
BST<T> BST<T>::find(T const & t) const
{
    if(empty())
        throw std::logic_error("BST<T>::find() - Empty / val not found");
    
    if(value() == t)
        return *this;
    
    return (t <= value() ? left().find(t) : right().find(t));
}

template <typename T>
std::size_t BST<T>::size() const
{
    return fold(0, [](int treeSize, T val){ return treeSize + 1;});
}

template <typename T>
std::size_t BST<T>::height() const
{
    if(empty())
        return 0;
    
    return 1 + std::max(BST<T>(m_root->m_left ).height(), 
                        BST<T>(m_root->m_right).height());
}

template <typename T>
BST<T> spine(BST<T> tree)
{
    auto f = [](BST<T> tree, T value) {return BST<T>(value, tree, BST<T>());};
    return tree.fold(BST<T>(), f);
}

template <typename T>
BST<T> operator+(T value, BST<T> tree)
{
    if(tree.empty())
        return BST<T>(value, BST<T>(), BST<T>());
    else if(value <= tree.value())
        return BST<T>(tree.value(), value + tree.left(), tree.right());
    else
        return BST<T>(tree.value(), tree.left(), value + tree.right());
}

template <typename T>
BST<T> operator+(BST<T> tree, T value)
{
    return value + tree;
}

template <typename T>
std::ostream & operator<<(std::ostream & stream, BST<T> tree)
{
    tree.fold(0, [&stream](int p, T val) {stream << val << " "; return p;});
    return stream;
}

template <typename T>
T max_diff(BST<T> tree)
{
    if(tree.size() < 2)
        throw std::logic_error("BST<T>::max_diff() - Too small tree");
    
    // Wartości w akumulatorze:
    // bool - czy nie pierwsza wczytana liczba
    // T    - największa uzyskana dotąd różnica
    // T    - poprzednia wartość
    auto f = [](std::tuple<bool, T, T> tup, T val)
             {
                 const auto dif = val - std::get<2>(tup);
                 const auto nv = std::get<0>(tup) ? 
                                 std::max(dif, std::get<1>(tup)) : dif;
                 return std::make_tuple(true, nv, val);
             };
    auto tup = tree.fold(std::make_tuple(false, tree.value(), tree.value()), f);
    return std::get<1>(tup);
}

#endif
