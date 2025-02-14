Spotting
========

Spotting is a major, often dominant, mode of fire propagation 
particularly under hot, dry, windy conditions. Parameters that control 
spotting are specified on the ``&SPOTTING`` namelist group. Note that 
the current spotting model is different from the spotting model 
described in the `original ELMFIRE paper 
<https://doi.org/10.1016/j.firesaf.2013.08.014>`_

Spotting is disabled by default and can be enabled by setting 
``ENABLE_SPOTTING = .TRUE.`` in the ``&SPOTTING`` namelist group. 
Spotting distance is modeled as a lognormal distribution with the mean 
and standard deviation determined semi-empirically as a function of 
ambient wind speed and fireline intensity. Shown below is a sample 
spotting configuration:

.. code-block::

   &SPOTTING
   ENABLE_SPOTTING                      = .TRUE.
   CROWN_FIRE_SPOTTING_PERCENT          = 1.0
   ENABLE_SURFACE_FIRE_SPOTTING         = .TRUE.
   SURFACE_FIRE_SPOTTING_PERCENT(:)     = 1.0
   CRITICAL_SPOTTING_FIRELINE_INTENSITY = 2000.0
   SPOTTING_DISTRIBUTION_TYPE           = 'LOGNORMAL'
   MEAN_SPOTTING_DIST                   = 5.0
   SPOT_FLIN_EXP                        = 0.3
   SPOT_WS_EXP                          = 0.7
   NORMALIZED_SPOTTING_DIST_VARIANCE    = 250.0
   NEMBERS_MIN                          = 1
   NEMBERS_MAX                          = 1
   PIGN                                 = 100.0
   /

Mean spotting distance (:math:`{m}`) and its variance (:math:`{v}`) are 
related to fireline intensity (:math:`{\dot{Q}'}`) and 20-ft wind speed 
(:math:`{u_{20}}`) as:

.. math::

   m = a{\dot{Q}'^b}u^c_{20}
   v = md

The empirical parameters :math:`{a}`, :math:`{b}`, :math:`{c}`, and 
:math:`{d}` may be specified directly or estimated by automated 
calibration and correspond to keywords in the ``&SPOTTING`` namelist 
group as follows:

   a = ``MEAN_SPOTTING_DISTANCE``

   b = ``SPOT_FLIN_EXP``

   c = ``SPOT_WS_EXP`` 

   d = ``NORMALIZED_SPOTTING_DIST_VARIANCE``

The normalized mean (:math:`{\mu}`) and standard deviation 
(:math:`{\sigma}` of the lognormal distribution are then calculated from 
:math:`{m}` and :math:`{v}` as:

.. math::

   \mu = ln(\frac{m^2}{\sqrt{v + m^2}})
   \sigma = \sqrt{ln(1 + \frac{v}{m^2})}

Spotting distance (:math:`{x}`) is calculated probabilistically from a 
lognormal distribution:

.. math::

   f(x) = \frac{1}{\sqrt{{2\pi}\sigma x}}exp(-\frac{1}{2}(\frac{lnx-\mu}{\sigma})^2)

By default, when ``ENABLE_SPOTTING = .TRUE.``, only pixels that burn as 
passive or active crown fire trigger the spotting algorithm. The keyword 
``CROWN_FIRE_SPOTTING_PERCENT`` controls spotting initiation from 
passive/active crown fire pixels, meaning if it is set to 1.0 then 1 out 
of every 100 pixels that burn as crown fire will initiate the spotting 
algorithm. When the spotting algorithm is initiated, the number of 
embers that is generated is determined randomly as constrained by the 
keywords ``NEMBERS_MIN`` and ``NEMBERS_MAX``. More specifically, the 
number of embers generated will be greater than or equal to 
``NEMBERS_MIN`` and less than or equal to ``NEMBERS_MAX``.

For each ember that is generated, spotting distance is determined 
stochastically according to the probability density function given 
above. Each ember is then treated as a Lagrangian particle that moves 
under the influence of the wind direction field, with its landing 
location assigned when it has traveled the previously-determined 
spotting distance. Ignition probability is controlled by the keyword 
``PIGN``, which is the probability in percent that an ember, once it has 
landed, initiates a spot fire. To avoid allocating computational time to 
tracking embers that don't initiate spot fire, it is most 
computationally efficient to set ``PIGN`` to 100% and then set 
``CROWN_FIRE_SPOTTING_PERCENT`` to a small number. As an example, 
``CROWN_FIRE_SPOTTING_PERCENT = 50.0`` and ``PIGN = 10.0`` will produce 
comparable results to ``CROWN_FIRE_SPOTTING_PERCENT = 5.0`` and ``PIGN = 
100.0``, but the computational expense of the spotting algorithm is 
reduced by a factor of approximately 10x in the latter case.

Surface fire spotting may be enabled by setting 
``ENABLE_SURFACE_FIRE_SPOTTING = .TRUE.``. Ember generation is treated 
analogously to crown fire ember generation, except that the surface fire 
ember generation probability is controlled by the keyword 
``SURFACE_FIRE_SPOTTING_PERCENT(:)``. The index for the array 
``SURFACE_FIRE_SPOTTING_PERCENT(:)`` is fuel model number so that 
surface fire spotting initiation can be controlled by fuel model. As an 
example, the following lines would disable surface fire spotting for all 
fuel types except for fuel models 141 - 149 (shrub fuel models in the 
conventional US system):

.. code-block::

   SURFACE_FIRE_SPOTTING_PERCENT(  1:140) = 0.0
   SURFACE_FIRE_SPOTTING_PERCENT(141:149) = 1.0
   SURFACE_FIRE_SPOTTING_PERCENT(150:256) = 0.0

The keyword ``CRITICAL_SPOTTING_FIRELINE_INTENSITY`` (default value of 
0.0) is the fireline intensity in units of kW/m below which surface fire 
spotting does not occur. This parameter has no impact on crown fire 
spotting.

Particularly under high winds, the parameters that control spotting 
exert a significant influence on overall fire progression. For that 
reason, it is often desirable to treat these parameters stochastically 
and, for fire hindcasts, as calibration coefficients. Setting the 
parameter ``STOCHASTIC_SPOTTING = .TRUE.`` directs ELMFIRE to treat the 
parameters that control spotting stochastically and, when ``CALIBRATION 
= .TRUE.``, automatically adjust these spotting parameters are 
automatically adjusted to optimize agreement between modeled and 
observed fire perimeters. When ``STOCHASTIC_SPOTTING = .TRUE.``, the 
following eight parameters are randomly generated within a 
user-specified range as will be described later:

   * ``CROWN_FIRE_SPOTTING_PERCENT``
   * ``GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT``
   * ``MEAN_SPOTTING_DIST``
   * ``NEMBERS_MAX``
   * ``NORMALIZED_SPOTTING_DIST_VARIANCE``
   * ``PIGN``
   * ``SPOT_FLIN_EXP``
   * ``SPOT_WS_EXP``

Note that ``GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT`` is applied to all 
fuel models. Shown below is a ``&SPOTTING`` configuration with 
stochastic spotting enabled and the 16 parameters that constrain the 
allowable range of the eight spotting parameters in **bold**:

.. code-block::

   &SPOTTING
   ENABLE_SPOTTING                          = .TRUE.
   STOCHASTIC_SPOTTING                      = .TRUE.
   CROWN_FIRE_SPOTTING_PERCENT_MIN          = 0.2
   CROWN_FIRE_SPOTTING_PERCENT_MAX          = 0.8
   ENABLE_SURFACE_FIRE_SPOTTING             = .TRUE.
   GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN = 0.2 
   GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX = 0.8
   CRITICAL_SPOTTING_FIRELINE_INTENSITY     = 2000.0
   SPOTTING_DISTRIBUTION_TYPE               = 'LOGNORMAL'
   MEAN_SPOTTING_DIST_MIN                   = 5.0
   MEAN_SPOTTING_DIST_MAX                   = 10.0
   NORMALIZED_SPOTTING_DIST_VARIANCE_MIN    = 250.0
   NORMALIZED_SPOTTING_DIST_VARIANCE_MAX    = 600.0
   SPOT_WS_EXP_LO                           = 0.4
   SPOT_WS_EXP_HI                           = 0.7
   SPOT_FLIN_EXP_LO                         = 0.2
   SPOT_FLIN_EXP_HI                         = 0.4
   NEMBERS_MIN                              = 1
   NEMBERS_MAX_LO                           = 1
   NEMBERS_MAX_HI                           = 1
   PIGN_MIN                                 = 100.0
   PIGN_MAX                                 = 100.0
   /

