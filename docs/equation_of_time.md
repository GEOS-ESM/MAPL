# Equation of Time

> **Author:** Peter M. Norris  
> University Space Research Association, Columbia, Maryland, USA  
> Global Modeling and Assimilation Office, NASA GSFC, Greenbelt, Maryland, USA  
> <peter.m.norris@nasa.gov>

## Introduction

The earth rotates on its axis with a period $T_\ast$ called the *sidereal day* (after the Latin for
"star", since it is the rotation period of the earth with respect to distant stars). $T_\ast$ is
slightly shorter than the so-called *mean solar day*, or clock day, of duration $T_{\mathcal{M}} =$
86400 seconds. This is because the earth is a prograde planet, that is, it rotates about its axis in
the same sense (counterclockwise looking down on the North Pole) as it orbits the sun. Specifically,
say the sun crosses the meridian of some location at a particular time. And imagine there is a
distant star directly behind the sun at that moment. After one sidereal day the location will rotate
360$^\circ$ about the earth's axis and the distant star will again cross its meridian. But during
that time the earth will have moved a small counterclockwise distance around its orbit and so it will
take a small additional rotation of the earth for the sun to also cross the meridian and thereby
complete a *solar day*.

Put another way, a solar day is slightly longer than a sidereal day because the sun appears to move
slowly eastward across the celestial sphere with respect to distant stars as the year passes. The
path of this motion is called the *ecliptic*. Clearly, what governs the length of a solar day is the
apparent velocity of the sun along the ecliptic, or, more particularly, the equatorial component of
that velocity. But both the magnitude and equatorial component of the solar ecliptic velocity change
during the year, the former because the earth's orbit is elliptical, not circular, and the latter
because the earth's axis of rotation is tilted with respect to the orbital (ecliptic) plane. Thus the
length of a solar day changes during the year. While these factors cause only a small perturbation to
the length of the solar day (less than 30 seconds), the perturbations accumulate so that, at
different times of the year, apparent solar time ("sundial time") and mean solar time ("clock time")
can differ by as much as about 15 minutes. This difference is called the Equation of Time.

To be more rigorous, in the sequel, let ${\mathcal{E}}$ denote the earth, ${\mathcal{S}}$ the true
sun, and ${\mathcal{M}}$ a fictitious "mean sun" that moves *at constant eastward speed around the
celestial equator*, completing a full orbit in a year, namely in the period $Y T_{\mathcal{M}}$,
where $Y$ is the number of mean solar days in a year (e.g., 365.25). Thus, in one mean solar day,
$T_{\mathcal{M}}$, the mean sun has moved an angle $2\pi/Y$ eastward. Hence, beyond one full earth
revolution, period $T_\ast$, an additional earth rotation of
$(T_{\mathcal{M}}-T_\ast) \cdot 2\pi/T_\ast = 2\pi/Y$ is required to "catch up with the moving sun",
as described earlier. Hence $T_{\mathcal{M}} - T_\ast = T_\ast / Y$ and so

```math
T_{\mathcal{M}} = T_\ast \frac{Y+1}{Y},
\tag{1}
```


a constant (near unity) multiple of the fixed sidereal day. $T_{\mathcal{M}}$ is the length of the
solar day for the "mean sun", or the "mean solar day". Because it is invariant during the year, it is
convenient for timekeeping, and forms the basis for "mean solar time", which at Greenwich is
essentially UTC. By *definition*, $T_{\mathcal{M}}$ = 24h = 86400s. That is, what we know as
"hours", "minutes" and "seconds", are just convenient integer fractions of the mean solar day. In
these units, the sidereal day $T_\ast$ is approximately 23h 56m 4s.

The solar zenith angle calculation (in `MAPL_sunGetInsolation`) needs the *local solar hour angle*,
$h_{\mathcal{S}}$, which is the angle, measured westward along the equator, from the local meridian
to the sun. This is just the *Greenwich solar hour angle*, $H_{\mathcal{S}}$, plus the longitude, so
we will henceforth work exclusively with Greenwich hour angles. We should use the hour angle of the
true sun, $H_{\mathcal{S}}$, but a common approximation replaces this with the hour angle of the
mean sun

```math
H_{\mathcal{M}} = 2\pi(u - 1/2),
\tag{2}
```


where $u$ is UTC time (in days) and the offset is needed because the mean solar hour angle is zero at
"noon". If more accuracy is required, the hour angle of the true sun is typically obtained as a small
correction to $H_{\mathcal{M}}$ called the Equation of time, $EOT$:

```math
H_{\mathcal{S}} = H_{\mathcal{M}} + EOT,  \quad\text{where}\quad  EOT = H_{\mathcal{S}} - H_{\mathcal{M}}.
\tag{3}
```


As discussed above, EOT corrects for two factors:

- the variable speed of the earth in its elliptical orbit about the sun (e.g., moving fastest at perihelion), and
- the tilt of the earth's axis of rotation with respect to its orbital plane (the "obliquity"), which
  causes the equatorial projection of the sun's apparent ecliptic motion to vary with the season
  (e.g., being parallel to the equator at the solstices.)

## Derivation of Equation of Time

We can write

```math
H_{\mathcal{S}} = H_{\Upsilon} - (H_{\Upsilon} - H_{\mathcal{S}}) = H_{\Upsilon} - \alpha_{\mathcal{S}},
```


where $H_{\Upsilon}$ is the Greenwich hour angle of the First Point of Aries (the location of the
vernal equinox, denoted $\Upsilon$) and is also known as the Greenwich sidereal hour angle, and
where $\alpha_{\mathcal{S}}$ is the right ascension of the true sun (since the right ascension of
any object is just the difference between the hour angles of $\Upsilon$ and the object). Hence,

```math
EOT = H_{\Upsilon} - H_{\mathcal{M}} - \alpha_{\mathcal{S}}.
\tag{4}
```


All three terms on the right of (4) are time variable: $\alpha_{\mathcal{S}}$ changes slowly
throughout the year, and is known from the earth-sun two-body elliptical orbit solution, while
$H_{\Upsilon}$ and $H_{\mathcal{M}}$ vary rapidly with Earth's rotation. $H_{\mathcal{M}}$ has a
period of one mean solar day, $T_{\mathcal{M}}$, and $H_{\Upsilon}$ has a period of one sidereal
day, $T_\ast$.

It may seem from (2) that the mean sun and its hour angle are fully specified. That, in fact, is not
yet the case: (2) is really just a definition of UTC, namely, that one UTC day is one mean solar day
and that the time of culmination of the mean sun, what we call "noon", occurs at UTC 12h00m. What we
are still at liberty to do is specify the phasing of the mean sun in its equatorial orbit, e.g., by
specifying the time $u_R$ at which the mean sun passes through $\Upsilon$ (both on the equator). At
this time, $H_{\Upsilon}(u_R) = H_{\mathcal{M}}(u_R)$, and so

```math
\begin{split}
H_{\Upsilon}(u) - H_{\mathcal{M}}(u)
& = 2\pi (u - u_R)(Y+1)/Y - 2\pi(u - u_R) \\
& = 2\pi(u - u_R) / Y \\
& = \mathrm{MA}(u) - \mathrm{MA}(u_R),
\end{split}
\tag{5}
```


where

```math
\mathrm{MA}(u) \equiv 2\pi(u - u_{\mathcal{P}}) / Y
\tag{6}
```


is the so-called "mean anomaly", known from the earth-sun two-body orbital solution, and
$u_{\mathcal{P}}$ is the time of perihelion. Thus, to fully determine $EOT$, through (4) and (5), we
need only to specify $\mathrm{MA}(u_R)$.

To understand the mean anomaly $\mathrm{MA}$, consider the standard two-body earth-sun problem in
which the earth ${\mathcal{E}}$ moves in an elliptical orbit about the sun ${\mathcal{S}}$ at one
focus, all in the *ecliptic plane*. The point on the ellipse closest to ${\mathcal{S}}$ is called
the perihelion ${\mathcal{P}}$. Obviously, the center of the ellipse ${\mathcal{O}}$, the focus
${\mathcal{S}}$ and the perihelion ${\mathcal{P}}$ are co-linear, the so-called major axis of the
ellipse. Additionally, let ${\mathcal{C}}$ be the circumscribing circle of the ellipse, with center
${\mathcal{O}}$ and passing through ${\mathcal{P}}$ (and the corresponding aphelion ${\mathcal{A}}$).
By Kepler's Second Law, the sun-earth vector sweeps out equal areas in equal times, so the
*fractional area* of the elliptical sector ${\mathcal{P}}{\mathcal{S}}{\mathcal{E}}$ is a linear
function of time, being zero at perihelion and one a year later. Specifically, this fractional area
is none other than the scaled mean anomaly
$\mathrm{MA}(u) / (2\pi) = (u - u_{\mathcal{P}}) / Y$ from (6). Clearly $\mathrm{MA}(u)$ can also
be interpreted as an angle, the angle $\angle{\mathcal{P}}{\mathcal{O}}{\mathcal{Q}}$ of a point
${\mathcal{Q}}$ orbiting on the circumcircle ${\mathcal{C}}$ at constant speed in the same direction
as the earth, also with a yearly period, and passing through ${\mathcal{P}}$ at the same time
$u_{\mathcal{P}}$ as the earth. Thus the point ${\mathcal{Q}}$ can be conceptualized as a sort of
"mean earth" orbiting a "second mean sun" (different from ${\mathcal{M}}$ above) at ${\mathcal{O}}$.
Note that while the angle $\mathrm{MA}(u) = \angle{\mathcal{P}}{\mathcal{O}}{\mathcal{Q}}$ of this
mean earth at time $u$ is a linear function of time, the corresponding angle of the real earth,
namely $\mathrm{TA}(u) \equiv \angle{\mathcal{P}}{\mathcal{S}}{\mathcal{E}}$, called the *true
anomaly*, is a non-linear function of time, since the real earth has a variable speed in its
elliptical orbit, e.g., moving faster at perihelion, so that its *areal fraction* is linear in time.
The relationship between $\mathrm{MA}(u)$ and $\mathrm{TA}(u)$ is known from the orbital solution
and will be discussed later. Finally, the *ecliptic longitude of the earth*,
$\lambda \equiv \angle\Upsilon{\mathcal{S}}{\mathcal{E}}$ is the angle at the sun, measured in the
same direction as the earth's motion, from the First Point of Aries $\Upsilon$ to the earth. Then

```math
\mathrm{TA}(u) \equiv \angle{\mathcal{P}}{\mathcal{S}}{\mathcal{E}}(u) = \angle{\mathcal{P}}{\mathcal{S}}\Upsilon + \angle\Upsilon{\mathcal{S}}{\mathcal{E}}(u) = \lambda(u) - \lambda_{\mathcal{P}},
```


where $\lambda_{\mathcal{P}} = \lambda(u_{\mathcal{P}}) \equiv \angle\Upsilon{\mathcal{S}}{\mathcal{P}} = -\angle{\mathcal{P}}{\mathcal{S}}\Upsilon$ is known as the *longitude of perihelion*, and is currently about $283^\circ$, or equivalently $-77^\circ$.

With this background, we can understand the quantity $\mathrm{MA}(u_R)$ we are trying to specify. If we ***choose***

```math
\mathrm{MA}(u_R) = - \lambda_{\mathcal{P}} = \angle{\mathcal{P}}{\mathcal{S}}\Upsilon \iff \angle{\mathcal{P}}{\mathcal{O}}{\mathcal{Q}}(u_R) = \angle{\mathcal{P}}{\mathcal{S}}\Upsilon,
\tag{7}
```


then at $u_R$, viewed from the mean earth ${\mathcal{Q}}$, the second (ecliptic) mean sun ${\mathcal{O}}$ is in direction of $\Upsilon$. And at that same time, by definition of $u_R$, the first (equatorial) mean sun ${\mathcal{M}}$, as seen from the real earth ${\mathcal{E}}$, is also in direction of $\Upsilon$.

## Integrals

Does this particular choice of $u_R$ give zero mean $EOT$, as required for a *mean* solar time?
Let $\langle\cdot\rangle$ denote a time average over one orbit (one year), so that (4), (5) and (7)
yield

```math
\langle EOT \rangle
= \langle \mathrm{MA}(u) \rangle + \lambda_{\mathcal{P}} - \langle \alpha_{\mathcal{S}} \rangle
= \mathrm{MA}(\langle u \rangle) + \lambda_{\mathcal{P}} - \langle \alpha_{\mathcal{S}} \rangle,
```


since $\mathrm{MA}$ is a linear function of $u$. In particular, let

```math
{\langle f \rangle}_X \equiv Y^{-1} \int_{X-Y/2}^{X+Y/2} f(u) \,\mathrm{d}u,
\tag{8}
```


whence

```math
\langle EOT \rangle_{u_X} = \mathrm{MA}(u_X) + \lambda_{\mathcal{P}} - \langle \alpha_{\mathcal{S}} \rangle_{u_X}.
\tag{9}
```


For example,

```math
\langle EOT \rangle_{u_{\mathcal{P}}} = \lambda_{\mathcal{P}} - \langle \alpha_{\mathcal{S}} \rangle_{u_{\mathcal{P}}}, \quad\text{and}\quad
\langle EOT \rangle_{u_\Upsilon} = \mathrm{MA}(u_\Upsilon) + \lambda_{\mathcal{P}} - \langle \alpha_{\mathcal{S}} \rangle_{u_\Upsilon}.
\tag{10}
```


The right ascension of the true sun, $\alpha_{\mathcal{S}} \in (-\pi,+\pi]$, is given by

```math
\alpha_{\mathcal{S}} = \text{atan2}(\sin\lambda\,\cos\varepsilon,\; \cos\lambda),
```


where $\varepsilon$ is the earth's obliquity ($\approx 23.5^\circ$). Both $\lambda$ and
$\alpha_{\mathcal{S}}$ are zero at $\Upsilon$. To proceed, we will use the following rate of change
of $\lambda$ from the two-body theory:

```math
\frac{\mathrm{d}\lambda}{\mathrm{d}u} = \frac{\mathrm{d}\nu}{\mathrm{d}u}
  = \frac{2\pi}{Y}(1-e^2)^{-3/2}(1 + e\cos\nu)^2,
```


where $e$ is the eccentricity and $\nu \equiv \lambda(u) - \lambda_{\mathcal{P}}$ is shorthand for
the true anomaly of the earth, $\mathrm{TA}(u)$. Then, without being precise on limits for now,

```math
\langle \alpha_{\mathcal{S}} \rangle
= Y^{-1} \int \frac{\alpha_{\mathcal{S}} \,\mathrm{d}\lambda}{\mathrm{d}\lambda / \mathrm{d}u}
= \int \frac{\text{atan2}(\sin\lambda\,\cos\varepsilon,\; \cos\lambda)}{(1-e^2)^{-3/2}[1+e\cos(\lambda-\lambda_{\mathcal{P}})]^2} \frac{\mathrm{d}\lambda}{2\pi}.
```


Finally, just as $\Upsilon$ denotes the location of the vernal equinox, $\lambda = \alpha_{\mathcal{S}} = 0$, we will also use $\Upsilon'$ to denote the location of the autumnal equinox, $\lambda = \alpha_{\mathcal{S}} = \pm \pi$. In general, $u_\Upsilon - u_{\Upsilon'}$ is not exactly half a year.

### Zero obliquity

For the simple case where the obliquity is zero, $\cos\varepsilon=1$ and $\alpha_{\mathcal{S}} = \lambda = \nu + \lambda_{\mathcal{P}}$, and so

```math
\begin{split}
\langle \alpha_{\mathcal{S}} \rangle_{u_{\mathcal{P}}}
= & \int_{-\pi}^{+\pi} \frac{\nu+\lambda_{\mathcal{P}}}{(1-e^2)^{-3/2}[1+e\cos\nu]^2} \frac{\mathrm{d}\nu}{2\pi} \\
= & \frac{\lambda_{\mathcal{P}}}{2\pi} \int_{-\pi}^{+\pi} \frac{\mathrm{d}\nu}{(1-e^2)^{-3/2}[1+e\cos\nu]^2},
\end{split}
```


since perihelion and aphelion are half a year apart by symmetry and since the $\nu$ term is odd. The
true anomaly can be expressed in terms of the *eccentric anomaly* $E \in (-\pi,+\pi]$:

```math
\cos\nu = \frac{\cos E - e}{1 - e \cos E}
\quad\text{and}\quad
\sin\nu = \frac{\sqrt{1 - e^2} \sin E}{1 - e \cos E},
```


whence

```math
1 + e\cos\nu = \frac{1 - e^2}{1 - e \cos E}.
```


and

```math
-\sin\nu \frac{\mathrm{d}\nu}{\mathrm{d}E} = -\sin E \frac{1 - e^2}{(1 - e \cos E)^2}
\implies \frac{\mathrm{d}\nu}{\mathrm{d}E} = \frac{\sqrt{1 - e^2}}{1 - e \cos E}.
```


Hence,

```math
\langle \alpha_{\mathcal{S}} \rangle_{u_{\mathcal{P}}}
= \frac{\lambda_{\mathcal{P}}}{2\pi} \int_{-\pi}^{+\pi} (1 - e \cos E) \,\mathrm{d}E = \frac{\lambda_{\mathcal{P}}}{2\pi} \int_{-\pi}^{+\pi} \mathrm{d}M
= \lambda_{\mathcal{P}},
```


where $M \equiv E - e \sin E$. Hence, as required, $\langle EOT \rangle_{u_{\mathcal{P}}} = 0$ by
(10). Note that $M(u)$ is none other than $\mathrm{MA}(u)$, as per Kepler's Equation of the
two-body solution.

### Zero eccentricity

For zero eccentricity, $e=0$, we get the simple form

```math
\langle \alpha_{\mathcal{S}} \rangle = \int \text{atan2}(\sin\lambda\,\cos\varepsilon,\; \cos\lambda) \frac{\mathrm{d}\lambda}{2\pi},
```


and, in particular,

```math
\langle\alpha_{\mathcal{S}}\rangle_{u_{\Upsilon'}+Y/2}
= \int_{u_{\Upsilon'}}^{u_{\Upsilon'}+Y} \alpha_{\mathcal{S}}(u) \frac{\mathrm{d}u}{Y}
= \int_{-\pi}^{+\pi} \text{atan2}(\sin\lambda\,\cos\varepsilon,\; \cos\lambda) \frac{\mathrm{d}\lambda}{2\pi}
= 0,
```


since $\text{atan2}$ is odd in $\lambda$. Then, by (9),

```math
\langle EOT \rangle_{u_{\Upsilon'}+Y/2} = \mathrm{MA}(u_{\Upsilon'}+Y/2) + \lambda_{\mathcal{P}} - \langle \alpha_{\mathcal{S}} \rangle_{u_{\Upsilon'}+Y/2} = \mathrm{MA}(u_\Upsilon) + \lambda_{\mathcal{P}},
```


since $\Upsilon'$ and $\Upsilon$ *are* a half year apart for a circular ($e=0$) orbit. But also for a circular orbit, $\mathrm{MA}(u) = \mathrm{TA}(u) = \lambda(u) - \lambda_{\mathcal{P}}$, so

```math
\langle EOT \rangle_{u_{\Upsilon'}+Y/2} = (\lambda(u_\Upsilon) - \lambda_{\mathcal{P}}) + \lambda_{\mathcal{P}} = \lambda(u_\Upsilon) \equiv 0,
```


as required.

### General case

For the general case,

```math
\begin{split}
\langle\alpha_{\mathcal{S}}\rangle_{u_{\Upsilon'}+Y/2}
& = \int_{u_{\Upsilon'}}^{u_{\Upsilon'}+Y} \alpha_{\mathcal{S}} \frac{\mathrm{d}u}{Y}
  = \int_{-\pi}^{+\pi} \frac{\text{atan2}(\sin\lambda\,\cos\varepsilon,\; \cos\lambda)}{(1-e^2)^{-3/2}[1+e\cos(\lambda-\lambda_{\mathcal{P}})]^2} \frac{\mathrm{d}\lambda}{2\pi} \\
& = \int_0^{\pi} \frac{\text{atan2}(\sin\lambda\,\cos\varepsilon,\; \cos\lambda)}{(1-e^2)^{-3/2}} D(\lambda; \lambda_{\mathcal{P}}) \frac{\mathrm{d}\lambda}{2\pi},
\end{split}
```


since $\text{atan2}$ is odd in $\lambda$, where

```math
\begin{split}
D(\lambda; \lambda_{\mathcal{P}})
& \equiv \frac{1}{[1+e\cos(\lambda - \lambda_{\mathcal{P}})]^2} - \frac{1}{[1+e\cos(\lambda + \lambda_{\mathcal{P}})]^2} \\
& = \frac{[1+e\cos(\lambda + \lambda_{\mathcal{P}})]^2 - [1+e\cos(\lambda - \lambda_{\mathcal{P}})]^2}{[(1+e\cos(\lambda - \lambda_{\mathcal{P}}))(1+e\cos(\lambda + \lambda_{\mathcal{P}}))]^2} \\
& = \frac{2e(\cos(\lambda + \lambda_{\mathcal{P}}) - \cos(\lambda - \lambda_{\mathcal{P}})) + e^2(\cos^2(\lambda + \lambda_{\mathcal{P}}) - \cos^2(\lambda - \lambda_{\mathcal{P}}))}{[1 + e(\cos(\lambda - \lambda_{\mathcal{P}}) + \cos(\lambda + \lambda_{\mathcal{P}})) + e^2 \cos(\lambda - \lambda_{\mathcal{P}}) \cos(\lambda + \lambda_{\mathcal{P}})]^2} \\
& = \frac{-4e\sin\lambda\sin\lambda_{\mathcal{P}} - 4 e^2\cos\lambda\cos\lambda_{\mathcal{P}}\sin\lambda\sin\lambda_{\mathcal{P}}}{[1 + 2e\cos\lambda\cos\lambda_{\mathcal{P}} + e^2 (\cos^2\lambda\cos^2\lambda_{\mathcal{P}} - \sin^2\lambda\sin^2\lambda_{\mathcal{P}})]^2} \\
& = \frac{-4 e \sin\lambda\sin\lambda_{\mathcal{P}} (1 + e\cos\lambda\cos\lambda_{\mathcal{P}})}{[(1 + e\cos\lambda\cos\lambda_{\mathcal{P}})^2 - e^2 \sin^2\lambda\sin^2\lambda_{\mathcal{P}}]^2}.
\end{split}
```


Continuing with the reduction,

```math
\begin{split}
\langle \alpha_{\mathcal{S}}\rangle_{u_{\Upsilon'}+Y/2}
&= \int_0^{\pi/2} \Bigl[
    \text{atan2}(\sin\lambda\,\cos\varepsilon,\; \cos\lambda)\,\frac{D(\lambda; \lambda_{\mathcal{P}})}{(1-e^2)^{-3/2}} \\
&\hspace{2cm} + \text{atan2}(\cos\lambda\,\cos\varepsilon,\; -\sin\lambda)\,\frac{D(\lambda+\pi/2; \lambda_{\mathcal{P}})}{(1-e^2)^{-3/2}}
  \Bigr]\frac{\mathrm{d}\lambda}{2\pi} \\
&= \int_0^{\pi/2} \Bigl[
    \arctan(\tan\lambda\,\cos\varepsilon)\,\frac{D(\lambda; \lambda_{\mathcal{P}})}{(1-e^2)^{-3/2}} \\
&\hspace{2cm} + [\pi-\arctan(\cot\lambda\,\cos\varepsilon)]\,\frac{D(\lambda+\pi/2; \lambda_{\mathcal{P}})}{(1-e^2)^{-3/2}}
  \Bigr]\frac{\mathrm{d}\lambda}{2\pi},
\end{split}
```


where

```math
D(\lambda+\pi/2; \lambda_{\mathcal{P}}) = \frac{-4 e \cos\lambda\sin\lambda_{\mathcal{P}} (1 - e\sin\lambda\cos\lambda_{\mathcal{P}})}{[(1 - e\sin\lambda\cos\lambda_{\mathcal{P}})^2 - e^2 \cos^2\lambda\sin^2\lambda_{\mathcal{P}}]^2}.
```


We will attempt a solution by expanding in powers of $e$, since $e \approx 0.0167 \ll 1$. Clearly
for $e=0$ both $D$ terms are zero and we get our earlier special case result.

#### First order in $e$

To *first* order in $e$:

```math
\frac{D(\lambda; \lambda_{\mathcal{P}})}{(1-e^2)^{-3/2}} \approx -4 e \sin\lambda\sin\lambda_{\mathcal{P}}, \quad
\frac{D(\lambda+\pi/2; \lambda_{\mathcal{P}})}{(1-e^2)^{-3/2}} \approx -4 e \cos\lambda\sin\lambda_{\mathcal{P}},
```


and so

```math
\begin{split}
\langle \alpha_{\mathcal{S}}\rangle_{u_{\Upsilon'}+Y/2}
& \approx -4 e \sin\lambda_{\mathcal{P}} \int_0^{\pi/2} \bigl[
    \arctan(\tan\lambda\,\cos\varepsilon) \sin\lambda
    + (\pi-\arctan(\cot\lambda\,\cos\varepsilon)) \cos\lambda
  \bigr] \frac{\mathrm{d}\lambda}{2\pi} \\
& = \frac{-e \sin\lambda_{\mathcal{P}}}{\pi/2} \Big[
    \cot\varepsilon\,\text{artanh}(\sin\lambda\,\sin\varepsilon) - \cos\lambda \arctan(\tan\lambda\,\cos\varepsilon) \\
& \hspace{1.75cm}
    + \cot\varepsilon\,\text{artanh}(\cos\lambda\,\sin\varepsilon) - \sin\lambda \arctan(\cot\lambda\,\cos\varepsilon)
    + \pi \sin\lambda \Big]_0^{\pi/2} \\
& = \frac{-e \sin\lambda_{\mathcal{P}}}{\pi/2} \Big[
    \cot\varepsilon [\text{artanh}(\sin\varepsilon) - \text{artanh}(0)]
    - [0 \cdot \arctan(\infty) - \arctan(0)] \\
& \hspace{2cm}
    + \cot\varepsilon [\text{artanh}(0) - \text{artanh}(\sin\varepsilon)]
    - [\arctan(0) - 0 \cdot \arctan(\infty)] + \pi \Big] \\
& = -2e \sin\lambda_{\mathcal{P}}.
\end{split}
```


Now, by (9),

```math
\begin{split}
\langle EOT \rangle_{u_{\Upsilon'}+Y/2}
& = \mathrm{MA}(u_{\Upsilon'}+Y/2) + \lambda_{\mathcal{P}} - \langle \alpha_{\mathcal{S}} \rangle_{u_{\Upsilon'}+Y/2} \\
& = \mathrm{MA}(u_{\Upsilon'}) + \pi + \lambda_{\mathcal{P}} + 2e \sin\lambda_{\mathcal{P}} \\
& = E(u_{\Upsilon'}) - e \sin E(u_{\Upsilon'}) + \pi + \lambda_{\mathcal{P}} + 2e \sin\lambda_{\mathcal{P}} \\
& = 2\arctan\!\left[ \sqrt{\frac{1-e}{1+e}}\tan\!\left(\frac{\nu(u_{\Upsilon'})}{2}\right) \right]
    - \frac{e\sqrt{1-e^2}\sin\nu(u_{\Upsilon'})}{1 + e\cos\nu(u_{\Upsilon'})}
    + \pi + \lambda_{\mathcal{P}} + 2e \sin\lambda_{\mathcal{P}} \\
& = -2\arctan\!\left[ \sqrt{\frac{1-e}{1+e}}\tan\!\left(\frac{\lambda_{\mathcal{P}} + \pi}{2}\right) \right]
    + \frac{e\sqrt{1-e^2}\sin(\lambda_{\mathcal{P}} + \pi)}{1 + e\cos(\lambda_{\mathcal{P}} + \pi)}
    + \pi + \lambda_{\mathcal{P}} + 2e \sin\lambda_{\mathcal{P}} \\
& = -2\arctan\!\left[ \sqrt{\frac{1-e}{1+e}}\tan\!\left(\frac{\lambda_{\mathcal{P}} + \pi}{2}\right) \right]
    - \frac{e\sqrt{1-e^2}\sin\lambda_{\mathcal{P}}}{1 - e\cos\lambda_{\mathcal{P}}}
    + \pi + \lambda_{\mathcal{P}} + 2e \sin\lambda_{\mathcal{P}},
\end{split}
```


since $\mathrm{MA} = E - e\sin E$ and since the eccentric anomaly $E$ obeys the following relations
from the two-body solution,

```math
\sin E = \frac{\sqrt{1-e^2}\sin\nu}{1 + e\cos\nu}, \quad
\tan(E/2) = \sqrt{\frac{1-e}{1+e}}\tan(\nu/2),
```


with $\nu = \lambda - \lambda_{\mathcal{P}}$, and since $\nu(u_{\Upsilon'}) = \lambda(u_{\Upsilon'}) - \lambda_{\mathcal{P}} = -(\lambda_{\mathcal{P}} + \pi)$. Hence, to our first order in $e$ approximation,

```math
\begin{split}
\langle EOT \rangle_{u_{\Upsilon'}+Y/2}
& \approx -2\arctan\!\left[ (1-e)\tan\!\left(\frac{\lambda_{\mathcal{P}} + \pi}{2}\right) \right]
    - e\sin\lambda_{\mathcal{P}} + \pi + \lambda_{\mathcal{P}} + 2e \sin\lambda_{\mathcal{P}} \\
& \approx -2\arctan\!\left[ \tan\!\left(\frac{\lambda_{\mathcal{P}} + \pi}{2}\right) \right]
    + \frac{2e\tan\!\left(\frac{\lambda_{\mathcal{P}} + \pi}{2}\right)}{1+\tan^2\!\left(\frac{\lambda_{\mathcal{P}} + \pi}{2}\right)}
    + \pi + \lambda_{\mathcal{P}} + e \sin\lambda_{\mathcal{P}} \\
& = e\sin\!\left(\lambda_{\mathcal{P}} + \pi\right) + e \sin\lambda_{\mathcal{P}}
  = e [ \sin(\lambda_{\mathcal{P}} + \pi) + \sin\lambda_{\mathcal{P}} ] = 0,
\end{split}
```


as required.

We could proceed to higher order in $e$ by this method, but first we will try a slightly different
approach, using integration by parts, which will turn out to be easier.

## General case using integration by parts

Alternatively, we can integrate by parts:

```math
\begin{split}
\langle\alpha_{\mathcal{S}}\rangle_{u_{\Upsilon'}+Y/2}
& = \int_{M(u_{\Upsilon'})}^{M(u_{\Upsilon'})+2\pi} \alpha_{\mathcal{S}} \frac{\mathrm{d}M}{2\pi}
  = \frac{1}{2\pi} \left( \left[\alpha_{\mathcal{S}} M \right]_{u_{\Upsilon'}}^{u_{\Upsilon'}+Y} - \int_{-\pi}^{+\pi} M \,\mathrm{d}\alpha_{\mathcal{S}} \right) \\
& = M(u_{\Upsilon'})+\pi - \frac{1}{2\pi} \int_{-\pi}^{+\pi} M \frac{\mathrm{d}\alpha_{\mathcal{S}}}{\mathrm{d}\lambda} \,\mathrm{d}\lambda \\
& = M(u_{\Upsilon'})+\pi - \frac{1}{2\pi} \int_{-\pi}^{+\pi} \frac{(E - e \sin E)\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda \sin^2\varepsilon},
\end{split}
```


where (again) $M \equiv \mathrm{MA} = E - e \sin E$, and since $\alpha_{\mathcal{S}}(u_{\Upsilon'}) = \pm \pi$ and

```math
\frac{\partial}{\partial x} \text{atan2}(y,x) = \frac{-y}{x^2 + y^2} \quad\text{and}\quad
\frac{\partial}{\partial y} \text{atan2}(y,x) = \frac{x}{x^2 + y^2},
```


and so

```math
\frac{\mathrm{d}\alpha_{\mathcal{S}}}{\mathrm{d}\lambda}
= \frac{\mathrm{d}}{\mathrm{d}\lambda} \text{atan2}(\sin\lambda\,\cos\varepsilon,\; \cos\lambda)
= \frac{\cos\varepsilon}{1 - \sin^2\lambda\,\sin^2\varepsilon}.
\tag{11}
```


Hence, before making any approximation in the order of $e$, we have

```math
\begin{split}
\langle EOT \rangle_{u_{\Upsilon'}+Y/2}
& = \mathrm{MA}(u_{\Upsilon'}) + \pi + \lambda_{\mathcal{P}} - \langle \alpha_{\mathcal{S}} \rangle_{u_{\Upsilon'}+Y/2} \\
& = \lambda_{\mathcal{P}} + \frac{1}{2\pi} \int_{-\pi}^{+\pi} \frac{(E - e \sin E)\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& = \lambda_{\mathcal{P}} + \frac{1}{2\pi} \int_{-\pi}^{+\pi}
\frac{\left[2\arctan\!\left( \sqrt{\frac{1-e}{1+e}}\tan\!\left(\frac{\nu}{2}\right) \right) - \frac{e\sqrt{1-e^2}\sin\nu}{1 + e\cos\nu} \right] \cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon},
\end{split}
```


with $\nu = \lambda - \lambda_{\mathcal{P}}$. Clearly this method avoids explicit calculation of $\mathrm{MA}(u_{\Upsilon'})$.

#### First order in $e$

To *first* order in $e$:

```math
\begin{split}
\langle EOT \rangle_{u_{\Upsilon'}+Y/2}
& \approx \lambda_{\mathcal{P}} + \frac{1}{2\pi} \int_{-\pi}^{+\pi} \frac{\left[2\arctan\!\left( (1-e)\tan\!\left(\frac{\lambda - \lambda_{\mathcal{P}}}{2}\right) \right) - e\sin(\lambda - \lambda_{\mathcal{P}}) \right] \cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& \approx \lambda_{\mathcal{P}} + \frac{1}{2\pi} \int_{-\pi}^{+\pi} \frac{\left[\lambda - \lambda_{\mathcal{P}} - \frac{2e\tan\!\left(\frac{\lambda - \lambda_{\mathcal{P}}}{2}\right)}{1 + \tan^2\!\left(\frac{\lambda - \lambda_{\mathcal{P}}}{2}\right)} - e\sin(\lambda - \lambda_{\mathcal{P}}) \right] \cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& = \lambda_{\mathcal{P}} + \frac{1}{2\pi} \int_{-\pi}^{+\pi} \frac{\left[\lambda - \lambda_{\mathcal{P}} - 2e\sin(\lambda - \lambda_{\mathcal{P}}) \right] \cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& = \lambda_{\mathcal{P}} + \frac{1}{2\pi} \int_{-\pi}^{+\pi} \frac{\left[\lambda - \lambda_{\mathcal{P}} - 2e(\sin\lambda\cos\lambda_{\mathcal{P}} - \cos\lambda\sin\lambda_{\mathcal{P}}) \right] \cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& = \lambda_{\mathcal{P}} - \frac{1}{2\pi} \int_{-\pi}^{+\pi} \frac{\left[\lambda_{\mathcal{P}} - 2e\sin\lambda_{\mathcal{P}}\cos\lambda \right] \cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon},
\end{split}
```


after removing odd functions of $\lambda$ in the last line. By (11) we can simplify this to

```math
\begin{split}
\langle EOT \rangle_{u_{\Upsilon'}+Y/2}
& = \lambda_{\mathcal{P}} - \frac{\lambda_{\mathcal{P}}}{2\pi} \int_{-\pi}^{+\pi} \mathrm{d}\alpha_{\mathcal{S}}
    + \frac{2e\sin\lambda_{\mathcal{P}}}{2\pi} \int_{-\pi}^{+\pi} \frac{\cos\lambda\,\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& = \frac{2e\sin\lambda_{\mathcal{P}}}{2\pi} \int_0^0 \frac{\cos\varepsilon \;\mathrm{d}Y'}{1 - Y'^2\sin^2\varepsilon} = 0,
\quad \text{where } Y' \equiv \sin\lambda,
\end{split}
```


so again we have our required result to first order in $e$.

#### Higher orders in $e$

At this point we have not been able to go to higher orders in $e$. We cannot say whether these orders
will yield zero contributions to mean $EOT$ or not. Here is where the analysis leads, which perhaps
suggests that the mean $EOT$ is only zero to first order.

The following binomial series converge absolutely since $e \ll 1$:

```math
(1-e^2)^{1/2} = 1 + A, \quad A \equiv \sum_{k=1}^{\infty} \binom{1/2}{k} (-1)^k e^{2k} \sim O(e^2),
```

```math
(1-e^2)^{-1/2} = 1 + B, \quad B \equiv \sum_{k=1}^{\infty} \binom{-1/2}{k} (-1)^k e^{2k} \sim O(e^2),
```


and

```math
(1 + e\cos\nu)^{-1} = 1 + C, \quad C \equiv \sum_{k=1}^{\infty} \binom{-1}{k} e^k\cos^k\nu \sim O(e).
```


Then

```math
\sqrt{\frac{1-e}{1+e}} = (1-e)(1 + B) = 1 - e + D, \quad D \equiv (1-e) B \sim O(e^2),
```


and

```math
\frac{\sqrt{1-e^2}}{1 + e\cos\nu} = (1 + A)(1 + C) = 1 + C + A + AC.
```


So, to general order in $e$,

```math
\begin{split}
Q &\equiv 2\arctan\!\left( \sqrt{\frac{1-e}{1+e}}\tan\!\left(\frac{\nu}{2}\right) \right) - \frac{e\sqrt{1-e^2}\sin\nu}{1 + e\cos\nu} \\
& = 2\arctan\!\left( \tan\!\left(\frac{\nu}{2}\right) + (D - e)\tan\!\left(\frac{\nu}{2}\right) \right) - e(1 + C + A + AC)\sin\nu \\
& = \nu + (D - 2e - eA)\sin\nu - e(1 + A)C\sin\nu \\
& \quad + 2\sum_{n=2}^{\infty} \arctan^{(n)}\!\left( \tan\!\left(\frac{\nu}{2}\right) \right) \frac{(D - e)^n}{n!} \tan^n\!\left(\frac{\nu}{2}\right).
\end{split}
```


Hence,

```math
\begin{split}
\langle EOT \rangle_{u_{\Upsilon'}+Y/2}
& = \lambda_{\mathcal{P}} + \frac{1}{2\pi} \int_{-\pi}^{+\pi} \frac{Q\,\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& = \frac{1}{2\pi} \sum_{n=2}^{\infty} \frac{(D-e)^n}{n!}
    \int_{-\pi}^{+\pi} \frac{2\arctan^{(n)}\!\left(\tan\!\left(\frac{\lambda-\lambda_{\mathcal{P}}}{2}\right)\right)
    \tan^n\!\left(\frac{\lambda-\lambda_{\mathcal{P}}}{2}\right)\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& \quad - \frac{e(1+A)}{2\pi} \sum_{k=1}^{\infty} \binom{-1}{k} e^k
    \int_{-\pi}^{+\pi} \frac{\cos^k(\lambda-\lambda_{\mathcal{P}})\sin(\lambda-\lambda_{\mathcal{P}})\,\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon}.
\end{split}
\tag{12}
```


First look at the $\cos^k(\lambda-\lambda_{\mathcal{P}}) \sin(\lambda-\lambda_{\mathcal{P}})$ terms:

```math
\cos^k(\lambda-\lambda_{\mathcal{P}}) \sin(\lambda-\lambda_{\mathcal{P}}) = (C_1 C_{1p} + S_1 S_{1p})^k (S_1 C_{1p} - C_1 S_{1p}),
```


where $C_n \equiv \cos(n\lambda)$, $S_n \equiv \sin(n\lambda)$ and the $p$ subscripts denote
evaluation at $\lambda_{\mathcal{P}}$. For $k=1$,

```math
(C_1 C_{1p} + S_1 S_{1p})(S_1 C_{1p} - C_1 S_{1p}) = (S_2 C_{2p} - C_2 S_{2p}) / 2.
```


The first term is odd in $\lambda$ and will integrate to zero, but the second term is even and will
not be zero! So perhaps our result is only good to first order in $e$? The order $e^2$ component from
(12) for these terms is then:

```math
\begin{split}
& \frac{e^2}{2\pi} \int_{-\pi}^{+\pi} \frac{\cos(\lambda-\lambda_{\mathcal{P}}) \sin(\lambda-\lambda_{\mathcal{P}})\,\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& = -\frac{e^2\sin(2\lambda_{\mathcal{P}})}{4\pi} \int_{-\pi}^{+\pi} \frac{\cos(2\lambda)\,\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& \quad = \frac{e^2\sin(2\lambda_{\mathcal{P}})}{8\pi\sin^2\varepsilon}
    \Big[ (\cos(2\varepsilon) + 3) \arctan(\cos\varepsilon\tan\lambda) - 4\lambda\cos\varepsilon \Big]_{-\pi}^{+\pi} \\
& \quad = -e^2 \cot\varepsilon \csc\varepsilon \sin(2\lambda_{\mathcal{P}}).
\end{split}
\tag{13}
```


Likewise, the derivatives of $\arctan$ term of (12) for order $e^2$ is:

```math
\begin{split}
& \frac{e^2}{2} \cdot \frac{1}{2\pi} \int_{-\pi}^{+\pi}
    \frac{2\arctan^{(2)}\!\left(\tan\!\left(\frac{\lambda-\lambda_{\mathcal{P}}}{2}\right)\right)
    \tan^2\!\left(\frac{\lambda-\lambda_{\mathcal{P}}}{2}\right)\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& = -\frac{e^2}{2} \cdot \frac{1}{2\pi} \int_{-\pi}^{+\pi}
    \frac{\sin^2(\lambda-\lambda_{\mathcal{P}})\tan\!\left(\frac{\lambda-\lambda_{\mathcal{P}}}{2}\right)\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& = -\frac{e^2}{2} \cdot \frac{1}{2\pi} \int_{-\pi}^{+\pi}
    \frac{\sin(\lambda-\lambda_{\mathcal{P}})[1 - \cos(\lambda-\lambda_{\mathcal{P}})]\,\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon} \\
& = \frac{e^2}{2} \cdot \frac{1}{2\pi} \int_{-\pi}^{+\pi}
    \frac{\sin(\lambda-\lambda_{\mathcal{P}})\cos(\lambda-\lambda_{\mathcal{P}})\,\cos\varepsilon \;\mathrm{d}\lambda}{1 - \sin^2\lambda\,\sin^2\varepsilon},
\end{split}
\tag{14}
```


which is exactly half of (13). So, we conclude that to second order in $e$,

```math
\langle EOT \rangle_{u_{\Upsilon'}+Y/2} = -\tfrac{3}{2} e^2 \cot\varepsilon \csc\varepsilon \sin(2\lambda_{\mathcal{P}}).
```


We will not pursue any higher order terms. For typical J2000 values $e \approx 0.0167$,
$\varepsilon \approx 23.44^\circ$, and $\lambda_{\mathcal{P}} \approx 102.95^\circ - 180^\circ = -77.05^\circ$,
the above formula gives

```math
\langle EOT \rangle_{u_{\Upsilon'}+Y/2} \approx 0.00106\,\text{rad} \approx 15\,\text{sec}.
```


Thus, we conclude that our simple choice of $u_R$ in (7) leads to zero mean $EOT$ only to first order in $e$.
