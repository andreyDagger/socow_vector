#pragma once

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <memory>
#include <utility>

template <typename T, size_t SMALL_SIZE>
class socow_vector {
private:
  using size_t = std::size_t;

private:
  struct buffer {
    size_t _capacity{0};
    size_t _ref_count{1};
    T _data[0];
  };

public:
  using value_type = T;

  using reference = T&;
  using const_reference = const T&;

  using pointer = T*;
  using const_pointer = const T*;

  using iterator = pointer;
  using const_iterator = const_pointer;

public:
  socow_vector() noexcept : _buf(nullptr) {}

  socow_vector(const socow_vector& other) {
    *this = other;
  }

  socow_vector& operator=(const socow_vector& other) {
    if (this != &other) {
      if (is_small && other.is_small) {
        size_t min_size = std::min(size(), other.size());
        socow_vector tmp;
        tmp._size = std::uninitialized_copy_n(other.begin(), min_size, tmp.begin()) - tmp.begin();

        std::uninitialized_copy(other.begin() + min_size, other.end(), end());

        size_t prev_size = size();
        _size = std::max(size(), other.size());

        std::swap_ranges(tmp.begin(), tmp.begin() + min_size, begin());
        std::destroy(begin() + min_size, begin() + prev_size);

        _size = other.size();
        return *this;
      }

      if (is_small && !other.is_small) {
        destructive_clear();
        _buf = other._buf;
        add_ref();
      } else if (!is_small && other.is_small) {
        copy_to_small(other.begin(), other.end());
      } else if (!is_small && !other.is_small) {
        release_ref();
        _buf = other._buf;
        add_ref();
      }
    }

    _size = other.size();
    is_small = other.is_small;
    return *this;
  }

  ~socow_vector() noexcept {
    is_small ? destructive_clear() : release_ref();
  }

  reference operator[](size_t index) {
    return data()[index];
  }

  const_reference operator[](size_t index) const noexcept {
    return data()[index];
  }

  pointer data() {
    if (!is_small) {
      unshare();
      return _buf->_data;
    } else {
      return static_data;
    }
  }

  const_pointer data() const noexcept {
    return is_small ? static_data : _buf->_data;
  }

  size_t size() const noexcept {
    return _size;
  }

  reference front() {
    return data()[0];
  }

  const_reference front() const noexcept {
    return data()[0];
  }

  reference back() {
    return data()[_size - 1];
  }

  const_reference back() const noexcept {
    return data()[_size - 1];
  }

  void push_back(const T& value) {
    insert(std::as_const(*this).end(), value);
  }

  void pop_back() {
    assert(size() != 0);
    erase(std::as_const(*this).end() - 1);
  }

  bool empty() const noexcept {
    return size() == 0;
  }

  size_t capacity() const noexcept {
    return is_small ? SMALL_SIZE : _buf->_capacity;
  }

  void reserve(size_t new_capacity) {
    if (new_capacity <= size()) {
      return;
    }
    if (is_small) {
      if (new_capacity <= SMALL_SIZE) {
        return;
      }
      set_capacity(new_capacity);
    } else {
      if (new_capacity <= SMALL_SIZE) {
        make_small();
      } else {
        if (_buf->_ref_count == 1 && new_capacity < capacity()) {
          return;
        }
        set_capacity(new_capacity);
      }
    }
  }

  void shrink_to_fit() {
    if (is_small) {
      return;
    }
    if (size() <= SMALL_SIZE) {
      make_small();
    } else if (size() < capacity()) {
      set_capacity(size());
    }
  }

  void clear() {
    is_shared() ? release_ref() : destructive_clear();
    _size = 0;
  }

  void swap(socow_vector& other) {
    if (this == &other) {
      return;
    }
    if (other.is_small) {
      if (is_small) {
        if (other.size() < size()) {
          std::uninitialized_copy(static_data + other.size(), static_data + size(), other.static_data + other.size());
          std::destroy(static_data + other.size(), static_data + size());
        } else if (other.size() > size()) {
          std::uninitialized_copy(other.static_data + size(), other.static_data + other.size(), static_data + size());
          std::destroy(other.static_data + size(), other.static_data + other.size());
        }
        std::swap(is_small, other.is_small);
        std::swap(_size, other._size);
        std::swap_ranges(static_data, static_data + std::min(other.size(), size()), other.static_data);
      } else {
        other.swap(*this);
      }
    } else {
      socow_vector tmp = other;
      other = *this;
      *this = tmp;
    }
  }

  iterator begin() {
    return data();
  }

  iterator end() {
    return data() + size();
  }

  const_iterator begin() const noexcept {
    return data();
  }

  const_iterator end() const noexcept {
    return data() + size();
  }

  iterator insert(const_iterator pos, const T& value) {
    size_t idx = pos - std::as_const(*this).begin();
    if (size() == capacity() || is_shared()) {
      socow_vector tmp;
      tmp.set_capacity(capacity() == 0 ? 1 : size() * 2);
      std::uninitialized_copy_n(std::as_const(*this).begin(), idx, tmp.begin());
      tmp._size += idx;
      std::construct_at(tmp.data() + idx, value);
      tmp._size++;
      std::uninitialized_copy(std::as_const(*this).begin() + idx, std::as_const(*this).end(), tmp.begin() + idx + 1);
      tmp._size = size() + 1;
      *this = tmp;
    } else {
      std::construct_at(data() + size(), value);
      _size++;
      for (size_t i = size() - 1; i > idx; --i) {
        std::swap(data()[i], data()[i - 1]);
      }
    }
    return begin() + idx;
  }

  iterator erase(const_iterator pos) {
    return erase(pos, pos + 1);
  }

  iterator erase(const_iterator first, const_iterator last) {
    size_t idx = first - std::as_const(*this).begin();
    size_t start = last - std::as_const(*this).begin();
    size_t len = last - first;
    if (len == 0) {
      return begin() + idx;
    }
    if (!is_shared()) {
      for (size_t i = start; i != size(); ++i) {
        std::swap(data()[first + i - last], data()[i]);
      }
      std::destroy_n(end() - len, len);
      _size -= len;
    } else {
      socow_vector tmp;
      tmp.set_capacity(size() - len);
      for (size_t i = 0; i < size() - len; ++i) {
        if (i < idx) {
          tmp.push_back(std::as_const(*this)[i]);
        } else {
          tmp.push_back(std::as_const(*this)[i + len]);
        }
      }
      swap(tmp);
    }
    return begin() + idx;
  }

private:
  void set_capacity(size_t new_capacity) {
    buffer* new_buf = get_buf_initialized(new_capacity);
    if (is_small) {
      destructive_clear();
    } else {
      release_ref();
    }
    _buf = new_buf;
    is_small = false;
  }

  bool is_shared() const noexcept {
    return !is_small && _buf->_ref_count > 1;
  }

  void unshare() {
    unshare(capacity());
  }

  void unshare(size_t new_capacity) {
    if (!is_shared()) {
      return;
    }
    buffer* new_buf = get_buf_initialized(new_capacity);
    release_ref();
    _buf = new_buf;
  }

  void add_ref() noexcept {
    ++_buf->_ref_count;
  }

  void release_ref() noexcept {
    assert(!is_small);
    if (!_buf) {
      return;
    }

    if (_buf->_ref_count == 1) {
      delete_buf(_buf);
    } else {
      --_buf->_ref_count;
    }
    _buf = nullptr;
  }

  void destructive_clear() noexcept {
    std::destroy_n(begin(), size());
  }

  buffer* get_buf_initialized(size_t capacity) {
    std::byte* memory = new std::byte[sizeof(buffer) + sizeof(T) * capacity];
    buffer* res = nullptr;
    try {
      res = new (memory) buffer{capacity, 1, {}};
      std::uninitialized_copy_n(std::as_const(*this).begin(), std::min(capacity, size()), res->_data);
    } catch (...) {
      if (res != nullptr) {
        res->~buffer();
      }
      delete[] reinterpret_cast<std::byte*>(res);
      throw;
    }
    return res;
  }

  void delete_buf(buffer* other_buf) noexcept {
    std::destroy_n(other_buf->_data, size());
    other_buf->~buffer();
    delete[] reinterpret_cast<std::byte*>(other_buf);
  }

  void copy_to_small(const_iterator first, const_iterator last) {
    assert(last - first <= SMALL_SIZE && !is_small);
    buffer* _buf_copy = _buf;
    try {
      std::uninitialized_copy(first, last, static_data);
    } catch (...) {
      _buf = _buf_copy;
      throw;
    }
    if (--_buf_copy->_ref_count == 0) {
      delete_buf(_buf_copy);
    }
    is_small = true;
  }

  void make_small() {
    copy_to_small(std::as_const(*this).begin(), std::as_const(*this).end());
  }

private:
  size_t _size{0};
  bool is_small{true};

  union {
    buffer* _buf;
    T static_data[SMALL_SIZE];
  };
};
