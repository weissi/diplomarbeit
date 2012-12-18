Diplomarbeit / Master's Thesis
==============================

Diplomarbeit / Master's Thesis by Johannes Weiß <diplomarbeit@johannesweiss.eu>

Advisor: [Daniel Kraschewski](http://www.iks.kit.edu/index.php?id=kraschewski)


Abstract
========

Today, strong cryptography plays a very important role. Cryptography
is of crucial importance mainly, but not limited, to important transactions via
the Internet and other communication networks because the information should
reach its destination reliably, confidentially and with integrity. The
telematics research solves the reliability problem, but cryptography is used to
ensure confidentiality and integrity. Since the overall problem is very complex,
an increasing effort to use cryptographic primitives and protocols is
discoverable. The benefit of this component based architecture is the
possibility to prove the security of the components individually. Of course, the
proofs must take into account the composability of the components which enables
to build complex and secure systems from smaller building blocks.

This thesis concerns itself with cryptographic primitives for Secure
Multi-Party Computations (MPC). MPC are joint computations of a set of parties
which reveal the result of the computation to any party and nothing else. Every
party should only learn information that can be calculated from its own input
and the result. This thesis mainly deals with Oblivious Polynomial
Evaluation (OPE), a subset of general MPC. OPE allows two parties to jointly
evaluate a polynomial where the first party chooses the polynomial and learns
nothing. The second party chooses the node and only learns the evaluated result
of the polynomial at the chosen node. In addition to the evaluation of
polynomials which is an obvious use case of OPE, OPE has many interesting
applications, such as the share generation for Shamir's Secret Sharing.
The methodology of this thesis also applies to the larger class of
Secure Function Evaluation (SFE) which enables to securely evaluate
arbitrary functions.

This thesis examines various approaches, leading to the novel result of a
cryptographic protocol realizing OPE in linear time. The security of the
methodology is proved in the Universal Composability (UC) framework
which places very strict demands on the security of cryptographic protocols.

Alongside the theoretical debate, this thesis also features an efficient, secure
and working implementation which manifests the properties of the protocol
mentioned above.
