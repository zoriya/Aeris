import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/service.dart';
import 'package:flutter/cupertino.dart';
import 'package:aeris/src/aeris_api.dart';
import 'package:get_it/get_it.dart';

/// Provider class for Action listed in /about.json
class ActionCatalogueProvider extends ChangeNotifier {
  /// Tells if the provers has loaded data at least once
  final Map<Service, List<ActionTemplate>> _triggerTemplates = {};
  final Map<Service, List<ActionTemplate>> _reactionTemplates = {};
  Map<Service, List<ActionTemplate>> get triggerTemplates => _triggerTemplates;
  Map<Service, List<ActionTemplate>> get reactionTemplates =>
      _reactionTemplates;

  String removeServiceFromAName(String aName) {
    var words = aName.split('_');
    words.removeAt(0);
    return words.join();
  }

  void reloadCatalogue() {
    _triggerTemplates.clear();
    _reactionTemplates.clear();
    Service.all().forEach((element) {
      _triggerTemplates.putIfAbsent(element, () => []);
      _reactionTemplates.putIfAbsent(element, () => []);
    });
    GetIt.I<AerisAPI>().getAbout().then((about) {
      if (about.isEmpty || about == null) return;
      final services = (about['server'] as Map<String, dynamic>)['services'] as List<dynamic>;
      for (var serviceContent in services) {
        Service service = Service.factory(serviceContent['name']);
        for (var action in (serviceContent['actions'] as List)) {
          _triggerTemplates[service]!.add(
            ActionTemplate(
              name: action['name'],
              displayName: action['label']['en'], ///TODO use locale
              service: service,
              description: action['description']['en'], ///TODO use locale
              parameters: (action['params'] as List).map(
                (e) => ActionParameter(name: e['name'], description: e['description']['en'])///TODO use locale
              ).toList(),
              returnedValues: (action['returns'] as List).map(
                (e) => ActionParameter(name: e['name'], description: e['description']['en']) ///TODO use locale
              ).toList(),
            )
          );
        }
        for (var reaction in serviceContent['reactions']) {
          _reactionTemplates[service]!.add(
            ActionTemplate(
              displayName: reaction['label']['en'], ///TODO use locale
              name: reaction['name'],
              service: service,
              description: reaction['description']['en'], ///TODO use locale
              parameters: (reaction['params'] as List).map(
                (e) => ActionParameter(name: e['name'], description: e['description']['en']) ///TODO use locale
              ).toList(),
              returnedValues: (reaction['returns'] as List).map(
                (e) => ActionParameter(name: e['name'], description: e['description']['en']) ///TODO use locale
              ).toList(),
            )
          );
        }
      }
      notifyListeners();
    });
  }

  ActionCatalogueProvider() {
    reloadCatalogue();
  }
}
