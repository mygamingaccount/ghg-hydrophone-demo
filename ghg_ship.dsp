declare name "GHG Hydrophone Contact rotator";
import("stdfaust.lib");

// INPUT PARAMETERS
// Number of hydrophone elements.
// GHG had 2 groups of 24 hydrophones on each side
nElements = 24;
// Total length of a hydrophone group in meters.
// A longer group improves low-frequency beam width but limits the highest usable frequency
glen = 2.5;
// Speed of sound in m/s for calculating the delay
c = 1465;
// Attenuation in dB when the contact is in the bow or stern baffles or on the opposite side
baffle_attenuation = ba.db2linear(-15);

// Hydrophones pick up sound from contacts between 20°-160° on each side. 10° linear rolloff is used here.
// Linear equation given two points: ax+b = (y2-y1)/(x2-x1)*x + (x2y1-x1y2)/(x2-x1)
contact = _ / nElements * baffle <: par(h, nElements, one_hydrophone(h))
with {
    baffle =
        baffle_attenuation , // opposite side
        (a1*brg)+b1        : select2((brg>=10) & (brg<20)),
        1                  : select2((brg>=20) & (brg<160)),
        (a2*brg)+b2        : select2((brg>=160) & (brg<170)),
        baffle_attenuation : select2(brg>=170);
    a1 = (1-baffle_attenuation)/10;
    b1 = (20*baffle_attenuation-10)/10;
    a2 = (baffle_attenuation-1)/10;
    b2 = (170-160*baffle_attenuation)/10;
    brg = cbrg*ma.signum(ghg_dial);
};

// Hydrophone contact rotator - delays the incoming signal depending on the bearing using fractional delay
// Each hydrophone in the group is represented by a delay as the wavefront passes through the ship's hull
// fdelay used here is two delays whose lengths are n, n+1, interpolated, which provides excellent resuls
one_hydrophone(h) = de.fdelay(maxdelay_n, element_delay)
with {
    maxdelay = ma.SR*glen/c;
    maxdelay_n    = maxdelay * (max(h, nElements-h-1) / nElements); // longest delay for each element
    element_delay = maxdelay * abs(cos(brg)) * (index / nElements);
    index = h, nElements-h-1 : select2(cos(brg)<0); // flip the delay line when contact is aft of the beam
    brg = cbrg * ma.PI / 180; // contact bearing off the bow in rads
};

cbrg = hslider("Contact bearing",0,-180,180,0.001);
// In this file, compensator  bearing is only required for the baffle calculation
ghg_dial = hslider("Compensator bearing",0,-180,180,0.001);

process = contact;
