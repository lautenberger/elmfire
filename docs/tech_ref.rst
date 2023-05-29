.. _tech_ref:

Technical Reference
===================

ELMFIRE's mathematical formulation is described in the `original ELMFIRE 
paper <https://doi.org/10.1016/j.firesaf.2013.08.014>`_. The spread rate 
formulation is repeated below: 

* :ref:`Spread Rate Formulation <trspreadrate>`
    * :ref:`Basics of Eulerian level set methods <treulerbasics>`
    * :ref:`Vectoring the Rothermel model <trvectoring>`
    * :ref:`Spread Rate at other orientations <trorientation>`
    * :ref:`Correcting for slope <trslope>`
    * :ref:`Variation in fireline intensity <trfirelineintensity>`
    * :ref:`Linking crown and surface fire <trcrown>`

.. _trspreadrate:

Spread Rate Formulation
-----------------------

.. _treulerbasics:

Basics of Eulerian level set methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Eulerian level set methods, including that in ELMFIRE, involve 
numerically solving the following hyperbolic partial differential 
equation for the scalar variable :math:`{\phi}`:


.. math:: 

   \frac{\partial{\phi}}{\partial t} + {U_x}\frac{\partial{\phi}}{\partial x} + {U_y}\frac{\partial{\phi}}{\partial y} = 0


:math:`{\phi}` has no physical meaning except that the :math:`{\phi}` = 
:0 isopleth (or
level set) corresponds to the fire front. This provides a convenient way 
to track a curved surface, such as a fire front, on a regular grid. 
ELMFIRE integrates the governing PDE using a narrow-band formulation 
(Sethian, 1996) with a second-order Runge-Kutta method superbee flux 
limiters to prevent numerical oscillations.

A key part of calculating the spread rate along the fire front is the 
normal vector to the :math:`{\phi}` field:

.. math::

   \begin{gather}
   \hat{n} = \frac{1}{|\nabla\phi|}(\frac{\partial{\phi}}{\partial x}\hat{i} + \frac{\partial{\phi}}{\partial y}\hat{j}) = n_x\hat{i} + n_y\hat{j}\\
   |\nabla\phi| = \sqrt{(\frac{\partial{\phi}}{\partial x})^2 + (\frac{\partial{\phi}}{\partial y})^2}
   \end{gather}   

Now define :math:`{\theta_n}` as the angle to which the :math:`{\phi}` 
field normal vector points:

.. math::

   \begin{equation}
   \theta_n =
   \begin{cases}
      \frac{1}{2}\pi - \tan^{-1}(\frac{n_y}{n_x}), & \text{for}\ n_x\geq0 & \text{and}\ n_y\geq0\\
      \frac{3}{2}\pi + \tan^{-1}(\frac{n_y}{|n_x|}), & \text{for}\ n_x\leq0 & \text{and}\ n_y\geq0\\ 
      \frac{3}{2}\pi - \tan^{-1}(\frac{n_y}{n_x}), & \text{for}\ n_x\leq0 & \text{and}\ n_y\leq0\\
      \frac{1}{2}\pi - \tan^{-1}(\frac{|n_y|}{n_x}), & \text{for}\ n_x\geq0 & \text{and}\ n_y\leq0\\       
   \end{cases}
   \end{equation}

Although the above equation gives :math:`{\theta_n}` at all locations, 
not just the fire front, we will later use
:math:`{\theta_n}` at the fire front in combination with the direction 
:of maximum spread and
assumed elliptical dimensions to calculate the spread rate at any 
orientation relative to the direction of maximum spread.

.. _trvectoring:

Vectoring the Rothermel model to calculate head fire spread rate and direction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:math:`{U_x}` and :math:`{U_y}` represent the fire spread rate in the 
:projected :math:`{x}` and :math:`{y}` map coordinates, *i.e.* velocity 
:normal to the slope
corrected for local shape and aspect. As will be explained below,
:math:`{U_x}` and :math:`{U_y}` are calculated as a function of space
and time using the Rothermel (1972) surface fire spread model, the Cruz 
(2005) crown fire model, and elliptical dimensions from Richards (1995).

Due to an unfortunate nomenclature convention, :math:`{\phi}` is used in 
both the Rothermel equation and in the governing level set PDE; these 
variables are unrelated and should not be confused. Hereafter, when 
referring to :math:`{\phi}` we are referring to the Rothermel 
formulation unless otherwise noted. The Rothermel fire spread model 
assumes wind and slope are aligned and gives the surface fire spread 
rate parallel to the slope (:math:`{V_s}`) as:

.. math::

   \frac{V_s}{V_{s0}} = {\alpha} (1 + {\phi_s} + {\phi_w})

where the outputs from the Rothermel model include :math:`{V_{s0}}` (the 
surface fire spread rate under no wind/no slope conditions), 
:math:`{\phi_s}` (slope factor), and :math:`{\phi_w}` (wind factor). The 
parameter :math:`{\alpha}` (not a part of the Rothermel model but 
included in ELMFIRE to account for point source acceleration) is bounded 
such that 0 < :math:`{\alpha}` :math:`{\leq}` 1.

By assuming that the directional effects of wind and slope are 
independent, :math:`{\phi_x}` and :math:`{\phi_y}` (the :math:`{x}` and 
:math:`{y}` components of an "overall" :math:`{\phi}`) can be calculated 
as:

.. math::

   \begin{gather}
   \phi_x = \alpha(\phi_{w,x} + \phi_{s,x}) = \alpha[\phi_w\sin(\theta_w - \pi) + \phi_s\sin(\theta_a - \pi)]\\
   \phi_y = \alpha(\phi_{w,y} + \phi_{s,y}) = \alpha[\phi_w\cos(\theta_w - \pi) + \phi_s\cos(\theta_a - \pi)]
   \end{gather}

:math:`{\theta_w}` is the direction from which the wind is blowing and 
::math:`{\theta_a}` is the
topographical aspect. The magnitude of the surface fire spread rate in 
the direction of maximum spread parallel to the local slope is:

.. math::

   \begin{gather}
   |V_{DMS,||}| = V_{s0}(\alpha + |\phi|)\\
   |\phi| = \sqrt{\phi^2_x + \phi^2_y}
   \end{gather}

The direction of maximum spread (the direction toward which the fire 
spreads most rapidly, *i.e.* the head fire direction) is calculated as:

.. math::

   \begin{equation}
   \theta_{DMS} =
   \begin{cases}
      \frac{1}{2}\pi - \tan^{-1}(\frac{\phi_y}{\phi_x}), & \text{for}\ \phi_x>0 & \text{and}\ \phi_y\geq0\\
      \frac{3}{2}\pi + \tan^{-1}(\frac{\phi_y}{|\phi_x|}), & \text{for}\ \phi_x<0 & \text{and}\ \phi_y\geq0\\ 
      \frac{3}{2}\pi - \tan^{-1}(\frac{\phi_y}{\phi_x}), & \text{for}\ \phi_x<0 & \text{and}\ \phi_y<0\\
      \frac{1}{2}\pi + \tan^{-1}(\frac{|\phi_y|}{\phi_x}), & \text{for}\ \phi_x>0 & \text{and}\ \phi_y\leq0\\       
   \end{cases}
   \end{equation}

.. _trorientation:

Elliptical dimensions to calculate spread rate at other orientations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since the Rothermel model as formulated above only gives spread rate in 
the direction of maximum spread, additional analysis is needed to 
provide spread rate in all other directions along the fire perimeter. 
ELMFIRE adopts the widely used assumption that the fire front is 
elliptical and that each point along the fire front behaves as an 
independent elliptical "wavelet" (although the wavelet concept is more 
appropriate in a Lagrangian frame of reference than in an Eulerian frame 
of reference). This is sometimes referred to as Huygens Principle.

The first step is to estimate, based on the combination of wind and 
slope, the fire's length to width ratio (*L/W*). More accurately, *L/W* 
is calculated for an elliptical wavelet at each point along the fire 
front, meaning *L/W* varies along the fire perimeter due to variations 
in slope and wind. Following Anderson (1983) as modified by Finney in 
FARSITE, *L/W* is estimated as:

.. math::

   \begin{gather}
   L/W = min[0.936exp(0.2566U_{mf,e}) + 0.461exp(-0.1548U_{mf,e}) - 0.397, 8]
   \end{gather}

where :math:`{U_{mf,e}}` is the effective mid-flame wind speed in mph 
(calculated below). As in FARSITE, the maximum value of *L/W* is limited 
to 8 by default but in ELMFIRE this is a user-specifiable parameter. 
Since the above equation for *L/W* is computationally expensive and must 
be evaluated in a narrow band surrounding the fire front twice per time 
step, ELMFIRE implements is as a factored third order polynomial.

Effective mid-flame wind speed is then calculated by solving Equation 47 
from Rothermel (1972) for the effective mid-flame wind speed as:

.. math::

   \begin{equation}
   U_{mf,e} = (\frac{|\phi|}{C(\frac{\beta}{\beta_{op}})^{-E}})^\frac{1}{B}
   \end{equation}

The parameters :math:`{C}`, :math:`{\beta}`, :math:`{\beta_{op}}`, 
:math:`{E}`, and :math:`{B}` are defined in Rothermel (1972) and can be 
thought of as parameters that depend on fuel model.

.. note::

   The above equation gives mid-flame wind-speed in ft/min whereas units 
   of mph are needed to calculate *L/W*. Also note that 
   :math:`{U_{mf,e}}` includes both slope and wind because as shown 
   earlier :math:`{|\phi|}` includes effects of both slope and wind.

Richards (1995) presented the mathematics necessary to calculate the 
spread rate of an elliptical fire (and elliptical wavelets at any point 
along the fire front). :math:`{\theta_n}` is the angle normal to the 
scalar :math:`{\phi}` field that forms the basis of the level set method 
(not the Rothermel :math:`{\phi}`). Define :math:`{\omega}` as the 
difference between the fire front normal and the direction of maximum 
spread:

.. math::

   \omega = \theta_n - \theta_{DMS}

In an orthogonal coordinate system oriented such that the :math:`{+y}` 
direction is aligned with the direction of maximum spread, velocity 
components in the :math:`{x}` and :math:`{y}` directions parallel to the 
local slope are:

.. math::

   \begin{gather}
   U^*_{y,||} = \frac{a^2\cos(\omega)}{\sqrt{a^2\cos^2(\omega) + b^2\sin^2(\omega)}} + c \\
   U^*_{x,||} = \frac{b^2\sin(\omega)}{\sqrt{a^2\cos^2(\omega) + b^2\sin^2(\omega)}} \\
   b = \frac{1}{2}\frac{|V_{DMS,||}| + V_{s0}}{\frac{L}{W}} \\
   a = \frac{1}{2}(|V_{DMS,||}| + V_{s0}) \\
   c = \frac{1}{2}(|V_{DMS,||}| - V_{s0})
   \end{gather}

The above equations assume that the fire backs at the no-wind/no-slope 
spread rate (:math:`{V_{s0}}`) and the asterisk denotes that the 
:math:`{x}` and :math:`{y}` velocities are relative to a coordinate 
system with the :math:`{+y}` direction aligned with the direction of 
maximum spread. ELMFIRE includes an option to determine backing spread 
rate from elliptical dimensions but by default the backing spread rate 
is taken as the no-wind/no-slope spread rate.

Since the velocity components calculated above are relative to a 
coordinate system with the :math:`{+y}` axis aligned with the direction 
of maximum spread, we have to rotate these velocity components to our 
map coordinate system as follows:

.. math::

   \begin{gather}
   U_{x,||} = U^*_{y,||}\sin(\theta_{DMS}) + U^*_{X,||}\cos(\theta_{DMS}) \\
   U_{y,||} = U^*_{y,||}\cos(\theta_{DMS}) - U^*_{X,||}\sin(\theta_{DMS}) \\
   \end{gather}

.. _trslope:

Correcting for slope
~~~~~~~~~~~~~~~~~~~~

Now we're almost done! The last step is to correct for slope because our 
grip is a projected map coordinate system, not a coordinate system 
aligned with the local slope. :math:`{U_x}` and :math:`{U_y}` are 
calculated as follows:

.. math::

   \begin{gather}
   \frac{U_x}{U_{x,||}} = 1 - |\sin(\theta_a)|(1 - \cos(\gamma)) \\
   \frac{U_y}{U_{y,||}} = 1 - |\cos(\theta_a)|(1 - \cos(\gamma))
   \end{gather}

Here :math:`{\gamma}` is the topographical slope. 

.. note::

   These equations have the correct limiting behavior: 

      * When :math:`{\gamma}` = 0 (flat terrain)
        :math:`{U_x}`/:math:`{U_{x,||}}` = 
        :math:`{U_y}`/:math:`{U_{y,||}}` = 1
      * Now take the case of an east-facing slope (:math:`{\theta_a}` = 
        :math:`{\pi/2}`. Since :math:`{\sin(\pi/2)}` = 1, 
        :math:`{U_x/U_{x,||}}` = :math:`{\cos(\gamma)}`. However, the 
        :math:`{y}`-direction spread rate is unaffected since 
        :math:`{\cos(\pi/2)}` = 0 and :math:`{U_y/U_{y,||}}` = 1.

.. _trfirelineintensity:

Variation in fireline intensity along the fire perimeter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since the Rothermel model only give fireline intensity (and flame length) 
in the head fire direction, how do we calculate these quantities in 
backing and flanking directions? Fireline intensity (*I*) is calculated 
as:

.. math::

   I = I_RR\tau

where :math:`{I_R}` is reaction intensity, :math:`{R}` is spread rate 
(we will define which spread rate momentarily), and :math:`{\tau}` is 
residence time. Neither :math:`{I_R}` nor :math:`{\tau}` depend on 
spread rate; therefore fireline intensity is only influenced by spread 
rate through :math:`{R}`. For the purposes of calculating fireline 
intensity (and subsequently flame length using Byram's equation) the 
correct definition of :math:`{R}` is:

.. math::

   R = \sqrt{U^2_{x,||} + U^2_{y,||}}

.. note::

   :math:`{R}` should be calculated from local :math:`{x}` and 
   ::math:`{y}` velocities
   parallel to the slope, *i.e.* before correcting for slope. 
   :math:`{R}` will vary along the fire perimeter with the highest value 
   in the heading direction and the lowest value in the backing 
   direction. Consequently, due to the definition of fireline intensity 
   above, fireline intensity will be highest in the heading direction, 
   lowest in the backing directions, and intermediate in flanking 
   directions.

.. _trcrown:

Linking crown and surface fire
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The above analysis is driven by the Rothermel surface fire spread model. 
Under conditions where canopy cover exceed 40% and fireline intensity 
exceeds critical fireline intensity, passive of active crown fire occurs 
per the Cruz 2005 correlation which can be expressed conceptually as 
follows:

.. math::

   V_C = f(U_{10},M_1,CBD)

where :math:`{V_C}` is the Cruz crown fire spread rate in the downwind 
direction, :math:`{U_{10}}` is the 10-meter wind speed,
:math:`{M_1}` is the 1-hour fine fuel moisture content, and 
::math:`{CBD}` is canopy bulk density. We link
the crown fire model to the Rothermel surface spread formulation through 
an effective wind factor (:math:`{\phi_{w,e}}`) defined as follows:

.. math::

   \phi_{w,e} = max(\phi_{w,R},\phi_{w,C})

where :math:`{\phi_{w,R}}` is the usual Rothermel wind factor and 
:math:`{\phi_{w,C}}` is a wind factor estimated from the Cruz crown fire 
model as follows:

.. math::

   \phi_{w,C} \approx \frac{V_C}{V_{s0}} - 1

The advantage of this formulation is that the crown fire spread rate can 
be linked to surface fire spread routing simply by modifying the value 
of :math:`{\phi_x}`.
