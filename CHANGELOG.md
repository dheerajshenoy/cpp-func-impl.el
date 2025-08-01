# CHANGELOG

## 0.1.2

- Region selection to select methods to implement `M-x cpp-func-impl-implement-region`
- DWIM (Do What I Mean) fashion command `M-x cpp-func-impl-implement-dwim`

## 0.1.1

- Handle `final`, `override` specifiers in method implementation
- Type specifier awareness (`constexpr`, `const`).
- Handle nested classes properly.
- Skip implementing header-bound declarations:
    + `inline`
    + `constexpr`
    + `consteval`
    + `constinit`
- Skip `default` and `delete` declarations

## 0.1

- Some knowledge about parsing nested class
- Method implementation insertion
- Commands: M-x
  + `cpp-func-impl-implement`
  + `cpp-func-impl-implement-all`
  + `cpp-func-impl-implement-selected`
  + `cpp-func-impl-concrete-class`
