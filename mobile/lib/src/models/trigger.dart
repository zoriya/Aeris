import 'package:flutter/material.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/action.dart' as aeris_action;

// Object representation of a pipeline trigger
class Trigger extends aeris_action.Action {

  /// Last time the triggered was done
  final DateTime last;
  const Trigger(
      {Key? key,
      required Service service,
      required String action,
      Object parameters = const {},
      required this.last}): super(service: service, name: action, parameters: parameters);
  // TODO Constructor from DB 'Type' field
  String lastToString() {
    int elapsedDays = DateTime.now().difference(last).inDays;
    return elapsedDays == 0
        ? 'Last: Today'
        : 'Last: ${elapsedDays.toString()}d ago';
  }
}
