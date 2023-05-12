# RISC-V based Spiking Neural Network Instruction Extension Design (RV-SNN)

RV-SNN is a RISC-V ISA based instruction extension aiming to accelerate the computing of SNN in general-purpose RISC-V processor, [Polaris](./Readme_Polaris.md).

This extension contains 3 parts:
- Neuron updating instruction: For LIF neuron computing;
- Synapse and general computing instructions: For synaptic plasticity computing, sum and exp function;
- SNN configuring instructions: For initializing SNN parameters, including rest voltage, time constant of LIF neuron, learning rate of synaptic plasticity algorithm and initial value of accumulation register.

The extended computing module is intergrated into the excutive stage in Polaris pipline.
