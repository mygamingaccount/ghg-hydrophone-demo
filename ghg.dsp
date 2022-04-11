declare name "GHG (Gruppenhörchgerät) Hydrophone Demo";
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
// Hydrophone contacts (ships)
nContacts = 6;
// Attenuation in dB when the contact is in the bow or stern baffles or on the opposite side
baffle_attenuation = ba.db2linear(-15);

// GHG
// Delay line emulating a GHG hydrophone group as it receives multiple simultaneous contacts
ghg = par(cn, nContacts, contact(cn)) :> si.bus(nElements);

// Hydrophones pick up sound from contacts between 20°-160° on each side. 10° linear rolloff is used here.
// Linear equation given two points: ax+b = (y2-y1)/(x2-x1)*x + (x2y1-x1y2)/(x2-x1)
contact(cn) = _ / nElements * baffle <: par(h, nElements, one_hydrophone(h,cn))
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
    brg = cbrg(cn)*ma.signum(ghg_dial);
};

// Hydrophone contact rotator - delays the incoming signal depending on the bearing using fractional delay
// Each hydrophone in the group is represented by a delay as the wavefront passes through the ship's hull
// fdelay used here is two delays whose lengths are n, n+1, interpolated, which provides excellent resuls
one_hydrophone(h,cn) = de.fdelay(maxdelay_n, element_delay)
with {
    maxdelay = ma.SR*glen/c;
    maxdelay_n    = maxdelay * (max(h, nElements-h-1) / nElements); // longest delay for each element
    element_delay = maxdelay * abs(cos(brg)) * (index / nElements);
    index = h, nElements-h-1 : select2(cos(brg)<0); // flip the delay line when contact is aft of the beam
    brg = cbrg(cn) * ma.PI / 180; // contact bearing off the bow in rads
};

// Compensator created using a simple delay line
// The Kriegsmarine stripline compensators used LC lowpass delay lines which had 17us group delay at DC.
// At 48000Hz one sample gives us 20.83us delay. I expect the performance to be very similar.
// TODO: Listening test at 58.8kHz sample rate to compare the performance of 17us vs 20.83us delays
compensator = si.bus(nElements) : par(n, nElements, stripline_delay(n)) :> _;

stripline_delay(n) = de.delay(maxdelay,strip_time)
with {
    maxdelay = ma.SR*glen/c;
    strip_time = maxdelay * abs(cos(brg)) * index/nElements;
    index = nElements-n-1, n: select2(cos(brg)<0);
    brg = ghg_dial * ma.PI / 180;
};

// For use with the Underwater Telegraph, a rectifier tube was added into the signal path according to the RN report on U-570
rectifier = _ <: _ , abs : select2(hgroup("Filters",hslider("Rectifier[style:radio{'Off':0;'On':1}]",0,0,1,1))) : _;

// Noise with variable amplitude to simulate wave and wind noise from the surface
wave_noise = (ba.db2linear(noise_constant_part)
        + ba.db2linear(noise_variable_part)
        * no.lfnoise(noise_wave_cutoff_hz)^2) // : hbargraph("Noise amplitude",0,0.2)
        * no.pink_noise : fi.lowpass(1,2500) : fi.highpass(1,1000)
with {
    noise_constant_part = -30;
    noise_variable_part = -20;
    noise_wave_cutoff_hz = 1;
};

// Misc UI elements
// debug:
//  : vgroup("",hbargraph("%n",0,200))
cbrg(i) = random_angle(i);
//cbrg(i) = hslider("S%i[style:knob][unit:°]",random_angle(i),-180,180,0.5) : si.smoo; // slider for the contact bearings

ghg_dial =
    vgroup("Bearing",
        hslider("Coarse[unit:°]",0,-180,180,1)
        + hslider("Fine[unit:°]",0,-5,5,0.01)
        : si.smoo
        * (button("Port/Starboard")*-2+1)
    );
hplist =
    hgroup("Filters",
        hslider(
            "Highpass[style:radio{'-':501;'500 Hz':500;'1000 Hz':1000;'3000 Hz':3000;'6000 Hz':6000;'10000 Hz':10000};]",
            501,10,10000,1)
        );
highpass_switch = _ <: _, fi.highpass(8,hplist) : select2(hplist != 501); // select2 otherwise it pops when you change the highpass parameters from lowest
// randomise the starting bearing of the contacts
random_angle = (_+30)*169691%360-180;

// Demonstration process configured for 6 sound sources, 2 outputs for stereo listening. 2nd order highpass represents the 
process = si.bus(nContacts) : ghg : compensator + wave_noise : rectifier : fi.highpass(1,100) : highpass_switch : fi.lowpass(10,7000) <: _,_;
