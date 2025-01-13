% Rule for battery issues
car_problem(battery_issue, [car_does_not_start, battery_is_dead]).

% Rule for starter motor issues
car_problem(starter_issue, [car_does_not_start, clicking_sound]).

% Rule for engine overheating
car_problem(coolant_issue, [engine_overheats, coolant_is_low]).

% Rule for low battery voltage
car_problem(low_voltage, [car_cranks_slowly]).

% Rule for old spark plugs
car_problem(old_spark_plugs, [engine_knocks]).

% Rule for alternator issue
car_problem(alternator_issue, [battery_warning_light_on, car_does_not_start, dim_headlights]).

% Rule for fuel pump failure
car_problem(fuel_pump_failure, [car_cranks_but_does_not_start, no_fuel_noise]).

% Rule for fuel filter clog
car_problem(fuel_filter_clog, [car_stalls, loss_of_power, engine_sputters]).

% Rule for ignition coil issue
car_problem(ignition_coil_issue, [engine_misfires, loss_of_power, check_engine_light_on]).

% Rule for low engine oil
car_problem(low_engine_oil, [engine_oil_light_on, engine_knocks, engine_overheats]).

% Rule for timing belt issue
car_problem(timing_belt_issue, [car_does_not_start, loud_ticking_sound]).

% Rule for radiator leak
car_problem(radiator_leak, [engine_overheats, coolant_is_low, visible_coolant_leak]).

% Rule for thermostat stuck
car_problem(thermostat_stuck, [engine_overheats, no_heat_in_cabin]).

% Rule for brake pad wear
car_problem(brake_pad_wear, [squeaking_brakes, poor_braking]).

% Explanation of issues
explanation(battery_issue, 'Check the battery connection, it might be loose or disconnected.').
explanation(starter_issue, 'Check the starter motor as it might be faulty.').
explanation(coolant_issue, 'Check for coolant leaks, as the engine might be overheating due to low coolant.').
explanation(low_voltage, 'The battery voltage might be low; please check the battery.').
explanation(old_spark_plugs, 'The spark plugs may be worn out and should be replaced.').
explanation(alternator_issue, 'The alternator may not be charging the battery.').
explanation(fuel_pump_failure, 'The fuel pump may not be functioning properly. Clean it out or hire a professional to perform maintainence.').
explanation(fuel_filter_clog, 'The fuel filter may be clogged, It is important to ensure it is clear or it could lead to further issues.').
explanation(ignition_coil_issue, 'The ignition coil may be failing, have a professional inspect it or do it yourself, and determine whether or not it needs to be replaced.').
explanation(low_engine_oil, 'Low oil levels in the engine. Check how much oil there is currently and replenish the oil levels accordingly.').
explanation(timing_belt_issue, 'The timing belt may be broken or misaligned. inspect and determine whether it should be realigned or replaced ').
explanation(radiator_leak, 'The radiator may be leaking.').
explanation(thermostat_stuck, 'The thermostat may be stuck closed.').
explanation(brake_pad_wear, 'The brake pads may be worn out. Get the replacedas soon as possible').

% Main rule to diagnose the problem based on exact symptomatic matches
diagnose :-
    car_problem(Problem, Symptoms),
    all_symptoms_present(Symptoms),
    explanation(Problem, Explanation),
    write('Exact Diagnosis: '), write(Problem), nl,
    write(Explanation), nl,
    !.
diagnose :-
    write('No exact diagnosis could be determined with the given symptoms.'), nl,
    suggest_diagnoses.

% Helper predicate to check if all symptoms in a list are present
all_symptoms_present([]).
all_symptoms_present([Symptom | Rest]) :-
    symptom(Symptom),
    all_symptoms_present(Rest).

% Function to suggest potential diagnoses based on partial matches
suggest_diagnoses :-
    write('Potential diagnoses based on partial matches:'), nl,
    findall(
        Score-Problem,
        (car_problem(Problem, RuleSymptoms), partial_match_score(RuleSymptoms, Score), Score > 0),
        Scores
    ),
    sort(1, @>=, Scores, SortedScores),
    print_suggestions(SortedScores).

% Helper predicate to calculate the partial match score
partial_match_score(RuleSymptoms, Score) :-
    findall(Symptom, symptom(Symptom), UserSymptoms),
    intersection(UserSymptoms, RuleSymptoms, MatchingSymptoms),
    length(MatchingSymptoms, Score).

% Helper predicate to print suggestions with explanations
print_suggestions([]) :-
    write(''), nl.
print_suggestions([Score-Problem | Rest]) :-
    explanation(Problem, Explanation),
    write(' - '), write(Problem), write(' (Match Score: '), write(Score), write(')'), nl,
    write('   '), write(Explanation), nl,
    print_suggestions(Rest).

% Function to clear existing symptoms
clear_symptoms :-
    retractall(symptom(_)).

% Function to add symptoms interactively
add_symptoms :-
    write('Enter a list of symptoms (e.g., [car_does_not_start, clicking_sound]): '),
    read(Symptoms),
    assert_symptoms(Symptoms).

% Helper function to assert symptoms from a list
assert_symptoms([]).
assert_symptoms([Symptom | Rest]) :-
    assert(symptom(Symptom)),
    assert_symptoms(Rest).
    
% Function to run the diagnosis process
run_diagnosis :-
    write('Car troubles that our system can diagnose:'), nl,
    write('battery_is_dead'), nl,
    write('battery_warning_light_on'), nl,
    write('car_cranks_but_does_not_start'), nl,
    write('car_cranks_slowly'), nl,
    write('car_does_not_start'), nl,
    write('car_stalls'), nl,
    write('check_engine_light_on'), nl,
    write('clicking_sound'), nl,
    write('coolant_is_low'), nl,
    write('dim_headlights'), nl,
    write('engine_knocks'),  nl,
    write('engine_misfires'), nl,
    write('engine_oil_light_on'), nl,
    write('engine_overheats'), nl,
    write('engine_sputters'), nl,
    write('loss_of_power'), nl,
    write('loud_ticking_sound'), nl,
    write('no_fuel_noise'),nl,
    write('no_heat_in_cabin'), nl,
    write('poor_braking'), nl,
    write('squeaking_brakes'), nl,
    write('visible_coolant_leak'), nl,
    clear_symptoms,
    add_symptoms,
    diagnose.
