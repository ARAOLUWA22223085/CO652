%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expert System for Car Fault Diagnosis 
% This system uses interactive yes/no questions
% to identify potential car faults.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define car problems and their associated symptoms
car_problem(battery_issue, [car_does_not_start, battery_is_dead]).
car_problem(starter_issue, [car_does_not_start, clicking_sound]).
car_problem(coolant_issue, [engine_overheats, coolant_is_low]).
car_problem(low_voltage, [car_cranks_slowly]).
car_problem(old_spark_plugs, [engine_knocks]).
car_problem(alternator_issue, [battery_warning_light_on, car_does_not_start, dim_headlights]).
car_problem(fuel_pump_failure, [car_cranks_but_does_not_start, no_fuel_noise]).
car_problem(fuel_filter_clog, [car_stalls, loss_of_power, engine_sputters]).
car_problem(ignition_coil_issue, [engine_misfires, loss_of_power, check_engine_light_on]).
car_problem(low_engine_oil, [engine_oil_light_on, engine_knocks, engine_overheats]).
car_problem(timing_belt_issue, [car_does_not_start, loud_ticking_sound]).
car_problem(radiator_leak, [engine_overheats, coolant_is_low, visible_coolant_leak]).
car_problem(thermostat_stuck, [engine_overheats, no_heat_in_cabin]).
car_problem(brake_pad_wear, [squeaking_brakes, poor_braking]).

% Explanation of each issue
explanation(battery_issue, 'The battery connection may be loose or the battery might be dead.').
explanation(starter_issue, 'The starter motor might be faulty and needs to be checked.').
explanation(coolant_issue, 'The engine is overheating due to low coolant levels or leaks.').
explanation(low_voltage, 'The battery voltage is too low; it may need recharging or replacement.').
explanation(old_spark_plugs, 'The spark plugs are worn out and need replacement.').
explanation(alternator_issue, 'The alternator is not charging the battery properly.').
explanation(fuel_pump_failure, 'The fuel pump may not be functioning correctly.').
explanation(fuel_filter_clog, 'The fuel filter is clogged and needs cleaning or replacement.').
explanation(ignition_coil_issue, 'The ignition coil is faulty and should be inspected or replaced.').
explanation(low_engine_oil, 'The engine oil level is low and needs replenishment.').
explanation(timing_belt_issue, 'The timing belt is broken or misaligned.').
explanation(radiator_leak, 'The radiator is leaking and should be repaired.').
explanation(thermostat_stuck, 'The thermostat is stuck closed and needs attention.').
explanation(brake_pad_wear, 'The brake pads are worn out and should be replaced.').

% Prompt the user with a yes/no question about a symptom
ask(Symptom) :-
    format('Does the car exhibit the following symptom: ~w? (yes/no): ', [Symptom]),
    read(Response),
    nl,
    (Response == yes -> assert(symptom(Symptom)); assert(not_symptom(Symptom))).

% Check if a symptom is present
check_symptom(Symptom) :- symptom(Symptom), !.
check_symptom(Symptom) :- not_symptom(Symptom), !, fail.
check_symptom(Symptom) :- ask(Symptom), check_symptom(Symptom).

% Diagnose the problem using backward chaining
% This predicate iterates through the list of problems and their symptoms
backward_diagnose :-
    car_problem(Problem, Symptoms),
    verify_symptoms(Symptoms),
    explanation(Problem, Explanation),
    format('Diagnosis: ~w~n', [Problem]),
    format('Explanation: ~w~n', [Explanation]),
    !.
backward_diagnose :-
    write('No conclusive diagnosis could be determined based on the symptoms provided.'), nl,
    partial_diagnoses.

% Verify if all symptoms of a problem are present
verify_symptoms([]).
verify_symptoms([Symptom | Rest]) :-
    check_symptom(Symptom),
    verify_symptoms(Rest).

% Provide potential diagnoses based on partial symptom matches with scores
partial_diagnoses :-
    write('Potential diagnoses based on partial symptom matches:'), nl,
    findall(
        Score-Problem,
        (car_problem(Problem, Symptoms), partial_match_score(Symptoms, Score), Score > 0),
        ScoredProblems
    ),
    sort(1, @>=, ScoredProblems, SortedProblems),
    print_partial_diagnoses(SortedProblems).

% Calculate partial match score for a problem
partial_match_score(Symptoms, Score) :-
    findall(Symptom, symptom(Symptom), UserSymptoms),
    intersection(UserSymptoms, Symptoms, MatchingSymptoms),
    length(MatchingSymptoms, MatchCount),
    length(Symptoms, TotalCount),
    Score is MatchCount / TotalCount.

% Print the potential diagnoses with their scores
print_partial_diagnoses([]) :-
    write('No partial matches found.'), nl.
print_partial_diagnoses([Score-Problem | Rest]) :-
    explanation(Problem, Explanation),
    format(' - ~w (Score: ~2f)~n', [Problem, Score]),
    format('   Explanation: ~w~n', [Explanation]),
    print_partial_diagnoses(Rest).

% Start the diagnosis process
start_diagnosis :-
    write('Welcome to the Car Fault Diagnosis Expert System!'), nl,
    write('Please answer the following questions with "yes" or "no" to identify the car problem.'), nl,
    retractall(symptom(_)),
    retractall(not_symptom(_)),
    backward_diagnose.

% Dynamic predicates to store user responses
:- dynamic symptom/1, not_symptom/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Usage Instructions
% 1. Load the file into a Prolog interpreter (e.g., SWI-Prolog).
% 2. Run the "start_diagnosis." predicate to begin.
% 3. Respond to the yes/no questions to allow the system to diagnose the car fault.
% 4. If no exact match is found, view potential diagnoses with their respective scores.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
