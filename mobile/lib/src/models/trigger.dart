// ignore_for_file: hash_and_equals

import 'package:aeris/src/models/action_parameter.dart';
import 'package:flutter/material.dart';
import 'package:aeris/src/main.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/models/action.dart' as aeris_action;
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:tuple/tuple.dart';

///Object representation of a pipeline trigger
class Trigger extends aeris_action.Action {
  /// Last time the triggered was done
  final DateTime? last;
  Trigger(
      {Key? key,
      required Service service,
      required String name,
      List<ActionParameter> parameters = const [],
      this.last})
      : super(service: service, name: name, parameters: parameters);

  /// Unserialize
  static Trigger fromJSON(Object action) {
    var triggerJSON = action as Map<String, Object>;
    Tuple2<Service, String> service =
        aeris_action.Action.parseServiceAndName(triggerJSON['pType'] as String);
    DateTime last = DateTime.parse(action['lastTrigger'] as String);

    return Trigger(
        service: service.item1,
        name: service.item2,
        last: last.year == 0 ? null : last,
        parameters: ActionParameter.fromJSON((triggerJSON['pParams'] as Map<String, Object>)['contents']
            as Map<String, Object>));
  }

  String lastToString() {
    var context = AppLocalizations.of(Aeris.materialKey.currentContext!);
    String lastStr = context.lastTrigger;
    if (last == null) return '$lastStr: ${context.never}';
    int elapsedDays = DateTime.now().difference(last!).inDays;
    return elapsedDays == 0
        ? '$lastStr: ${context.today}'
        : '$lastStr: $elapsedDays${context.nDaysAgo}';
  }

  /// Template trigger, used as an 'empty' trigger
  Trigger.template({Key? key, this.last})
      : super(service: Service.all()[0], name: '', parameters: []);

  @override
  // ignore: avoid_renaming_method_parameters
  bool operator ==(Object o) {
    Trigger other = o as Trigger;
    return service.name == other.service.name &&
        name == other.name &&
        last == other.last &&
        parameters.map((e) => e.name).toString() == other.parameters.map((e) => e.name).toString();
  }
}
