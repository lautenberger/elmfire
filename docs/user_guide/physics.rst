Physics and Numerics options
============================

Runtime options are specified via the ``&SIMULATOR`` namelist group. 
Brief descriptions of how various parameters influence physics options 
are presented below. See the ELMFIRE :ref:`tech_ref` for details.

.. ellipticaldim:

Elliptical dimensions
---------------------

In ELMFIRE, every point along the fire front behaves as an independent 
elliptical wavelet (Huygens principle). However, since the underlying 
surface fire spread model only provides the rate of spread in the head 
fire direction, the assumed elliptical fire shape is used to calculate 
spread rates in other directions. A key parameter is the ellipse's 
length to width ratio, which is estimated as a function of wind speed 
from an empirical correlation. The maximum allowable value of the length 
to width ratio is specified with the keyword ``MAX_LOW`` (default value 
of 8). Setting this to a lower value can prevent cigar shaped fires 
under high winds.

.. crownfire:

Crown fire
----------

The keyword ``CROWN_FIRE_MODEL`` can be used to enable or disable crown 
fire initiation and spread. By default, ``CROWN_FIRE_MODEL=1`` and crown 
fire spread rate is calculated from `Cruz et al. 2005. 
<https://cdnsciencepub.com/doi/abs/10.1139/x05-085>`_ Crown fire can be 
disabled by setting ``CROWN_FIRE_MODEL=0``. In certain cases, crown fire 
spread rates may be over-predicted, so an upper limit on spread rate can 
be specified via ``CROWN_FIRE_SPREAD_RATE_LIMIT``, which has a default 
value of 250 ft/min. Since crown fire may not always propagate in 
discontinuous canopies, the keyword ``CRITICAL_CANOPY_COVER`` is used to 
specify the minimum canopy cover at which crown fire occurs. The default 
value is 0.39 (note that this is a fraction, not a percent).

.. windflux:

Wind fluctuations
-----------------

Wind fluctuations, disabled by default, can be enabled by 
setting ``WIND_FLUCTUATIONS = .TRUE.``. Doing so directs ELMFIRE to 
perturb the wind field (speed and direction) in every cell in the 
computational domain every ``DT_WIND_FLUCTUATIONS`` seconds. The wind 
speed perturbation is the current wind speed multiplied by 
``WIND_SPEED_FLUCTUATION_INTENSITY`` multiplied by a randomly generated 
floating point number between -0.5 and +0.5. As an example, if the 
current wind speed is 10 mph and ``WIND_SPEED_FLUCTUATION_INTENSITY`` is 
0.2, then the perturbed (post-fluctuation) wind speed will be between 
9.0 mph and 11.0 mph. Wind direction fluctuations are implemented 
similarly, except that ``WIND_DIRECTION_FLUCTUATION_INTENSITY`` 
specifies the maximum magnitude of the wind direction fluctuation in 
degrees. For example, if the current wind direction is 90 degrees and 
``WIND_DIRECTION_FLUCTUATION_INTENSITY = 20.0``, then the perturbed 
(post-fluctuation) wind speed would be between 80-100 degrees. 
Essentially, ``WIND_SPEED_FLUCTUATION_INTENSITY`` is a relative value 
whereas ``WIND_DIRECTION_FLUCTUATION_INTENSITY`` is an absolute value in 
degrees.

.. misc:

Miscellaneous inputs
--------------------

Fire front expansion calculations are performed only in voxels/grid 
cells surrounding the fire front (narrow band level set method). The 
number of voxels on either side of the fire front is controlled by the 
parameter ``BANDTHICKNESS`` (default value of 2). It is normally 
unnecessary to adjust this parameter.

The parameter ``RANDOMIZE_RANDOM_SEED`` controls the seed used to 
initialize the random number generator. By default, 
``RANDOMIZE_RANDOM_SEED = .FALSE.`` and the random number generator is 
initialized using the ``SEED`` value as specified in the 
``&MONTE_CARLO`` namelist group. If ``RANDOMIZE_RANDOM_SEED`` is set to 
``.TRUE.``, then the random number seed is generated from the system 
clock.
