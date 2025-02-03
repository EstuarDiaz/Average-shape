# Average-shape
**Goal:** find the average shape of two countries. 

![](https://github.com/EstuarDiaz/Average-shape/blob/main/transition.gif)

**Method:** represent the contour of the countries using Fourier analysis and then do the average.

Let $f:\mathbb{R} \to \mathbb{C}$ be a complex valued periodic function, with period $2 \pi$. We can represent $f$ as a Fourier Series, given by 
$$
  f(t) = \frac{1}{2\pi}\sum\limits_{k=-\infty}^\infty c_k\cdot \exp{(k \cdot i t)}
$$

where each $c_k \in \mathbb{C}$ is defined as

$$ 
  c_k := \int_0^{2\pi} f(t)\exp{(-k \cdot  i t)} dt
$$

For this problem, we represent the contour of each country as a complex valued periodic function. Then, we compute all the coefficients of the Fourier Series. The average shape of the countries is given by taking the average of each of the coefficients in their respective Fourier series.
