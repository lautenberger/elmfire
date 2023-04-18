.. _verification:

Verification
============

ASTM E1355 *Standard Guide for Evaluating the Predictive Capability of 
Deterministic Fire Models* (2018) defines *model validation* as "the 
process of determining the degree to which a calculation method is an 
accurate representation of the real-world from the perspective of the 
intended uses of the calculation method."

ASTM E1355 *Standard Guide for Evaluating the Predictive Capability of 
Deterministic Fire Models* (2018) defines *model verification* as "the 
process of determining that the implementation of a calculation method 
accurately represents the developer's conceptual description of the 
calculation method and solution to the calculation method." Slightly 
different definitions of *model verification* are used by other 
standards in other industries, but in general verification involves 
assessing whether the underlying mathematical formulation is correctly 
implemented in the source code - or "is the math right". 

Model verification is typically approached by simulating simple 
canonical problems and comparing model outputs to exact solutions, where 
available. ELMFIRE has been subjected to extensive model verification 
exercises since development began in 2010. In an effort to promote 
transparency and authenticity, ELMFIRE's mathematical formulation is 
documented in the :ref:`Technical Reference<tech_ref>`, its source code 
is maintained in a `public Git repository 
<https://github.com/lautenberger/elmfire>`_, and several model 
verification test cases are presented below (with associated scripts 
and inputs available in the Git repo).

.. toctree::
   :maxdepth: 2

   verification/verification_01
   verification/verification_02
