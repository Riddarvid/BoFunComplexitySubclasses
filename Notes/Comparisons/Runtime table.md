- Alla mätningar har körts på Arvids laptop.
- Alla mätvärden är i sekunder.
- Canonization reduces the redundancy caused by D(f) == D(! . f)
- Alla dessa mätningar är gjorda genom att köra funktionen 3 gånger och ta medianvärdet. Inför den slutgiltiga rapporten bör vi köra betydligt många fler samples.

TODO: Testa alla -

### genAlgMemoThin

| **Function**    | Basic Gen    | Gen <br>with canonization | Gen <br>with normalization | Gen with both | Threshold  | Iterated Threshold | Symmetric   | Iterated Symmetric |
| --------------- | ------------ | ------------------------- | -------------------------- | ------------- | ---------- | ------------------ | ----------- | ------------------ |
|                 |              |                           |                            |               |            |                    |             |                    |
| **FlatMaj 11**  | 9.465031696  | 9.492267861               | 0.105690798                | 0.102600042   | 0.00071124 | 0.000197645        | 0.000974195 | 0.000242941        |
| **IterMaj 3 2** | 0.660037032s | 0.685222343               | 0.070498808                | 0.072914753   | N/A        | 0.008165325        | N/A         | 0.013071388        |
| **IterMaj 3 3** | -            | -                         | -                          | -             | N/A        | -                  | N/A         | -                  |

### computeMin

| **Function**    | Basic Gen   | Gen <br>with canonization | Gen <br>with normalization | Gen with both | Threshold   | Iterated Threshold | Symmetric   | Iterated Symmetric |
| --------------- | ----------- | ------------------------- | -------------------------- | ------------- | ----------- | ------------------ | ----------- | ------------------ |
|                 |             |                           |                            |               |             |                    |             |                    |
| **FlatMaj 11**  | 10.02142505 | 9.739210948               | 0.100104694                | 0.093713429   | 0.000911618 | 0.000204373        | 0.001135306 | 0.000383998        |
| **IterMaj 3 2** | 0.990276217 | 1.158129523               | 0.106123196                | 0.117547343   | N/A         | 0.013081782        | N/A         | 0.015872411        |
| **IterMaj 3 3** | -           | -                         | -                          | -             | N/A         | ~11 min            | N/A         | -                  |

### computeMin'

Note that computeMin' needs a Hashable instance which we have not yet been able to produce for Symm and Thresh.

Note also that the normalized version is the only one where flat 11 is faster than iterated 3 2, even though it has a higher arity.

| **Function**    | Basic Gen   | Gen <br>with canonization | Gen <br>with normalization | Gen with both |
| --------------- | ----------- | ------------------------- | -------------------------- | ------------- |
|                 |             |                           |                            |               |
| **FlatMaj 11**  | 1.36269634  | 1.444097572               | 0.028084827                | 0.029782328   |
| **IterMaj 3 2** | 0.573417156 | 0.579955031               | 0.074050395                | 0.072238293   |
| **IterMaj 3 3** | > 30 min    | -                         | -                          | -             |
