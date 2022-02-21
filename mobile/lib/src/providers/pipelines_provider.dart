import 'package:aeris/src/aeris_api.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:flutter/cupertino.dart';
import 'package:aeris/src/models/pipeline_collection.dart';
import 'package:get_it/get_it.dart';

/// Provider class for Pipelines
class PipelineProvider extends ChangeNotifier {
  /// List of Pipelines stored in Provider
  late PipelineCollection _pipelineCollection;

  /// Tells if the provers has loaded data at least once
  bool initialized = false;

  PipelineProvider() {
    _pipelineCollection = PipelineCollection(
        pipelines: [],
        sortingMethod: PipelineCollectionSort.last,
        sortingSplitDisabled: true);
    fetchPipelines();
  }

  /// Fetches the pipelines from API and put them in the collection
  Future<void> fetchPipelines() {
    return GetIt.I<AerisAPI>().getPipelines().then((pipelines) {
      _pipelineCollection.pipelines = pipelines;
      sortPipelines();
      initialized = true;
    });
  }

  /// Adds a pipeline in the Provider
  addPipeline(Pipeline newPipeline) {
    initialized = true;
    _pipelineCollection.pipelines.add(newPipeline);
    GetIt.I<AerisAPI>().createPipeline(newPipeline);
    sortPipelines();
    notifyListeners();
  }

  /// Sort pipelines inside the Provider, and notify listeners
  sortPipelines() {
    _pipelineCollection.sort();
    notifyListeners();
  }

  /// Removes a specific pipeline from the Provider, and notify listeners
  removePipeline(Pipeline pipeline) {
    _pipelineCollection.pipelines.remove(pipeline);
    GetIt.I<AerisAPI>().removePipeline(pipeline);
    notifyListeners();
  }

  /// Removes piplines matching predicates, and notify listeners
  removePipelinesWhere(bool Function(Pipeline) predicate) {
    List<Pipeline> toRemove =
        _pipelineCollection.pipelines.where(predicate).toList();
    for (Pipeline pipeline in toRemove) {
      GetIt.I<AerisAPI>().removePipeline(pipeline);
      _pipelineCollection.pipelines.remove(pipeline);
    }
    notifyListeners();
  }

  /// returns the number of piepliens currently in the collection
  int get pipelineCount => _pipelineCollection.pipelines.length;

  /// returns the pipeline a t the given index in the collection.
  /// Warning: if the pipeline is updated, no call will be made to api
  Pipeline getPipelineAt(int index) {
    return _pipelineCollection.pipelines[index];
  }

  PipelineCollectionSort get sortingMethod => _pipelineCollection.sortingMethod;

  /// Sets sorting method for pipelines, and sorts
  set sortingMethod(PipelineCollectionSort sortingMethod) {
    _pipelineCollection.sortingMethod = sortingMethod;
    sortPipelines();
  }

  /// Sets sorting method for pipelines, and sorts
  set splitDisabled(bool split) {
    _pipelineCollection.sortingSplitDisabled = split;
    sortPipelines();
  }

  /// Sets sorting method for pipelines, does not execute sorting
  bool get disabledSplit => _pipelineCollection.sortingSplitDisabled;
}
