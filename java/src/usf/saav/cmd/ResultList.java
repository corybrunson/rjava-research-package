package usf.saav.cmd;
import java.util.*;

public class ResultList {

    List<String> pTypes;
    List<String> vTypes;
    List<Float> pValues;
    List<Float> vValues;
    List<Float> pRealValues;
    List<Float> vRealValues;
    List<Integer> pGlobalIDs;
    List<Integer> vGlobalIDs;

    public ResultList() {
        pTypes = new ArrayList<>();
        vTypes = new ArrayList<>();
        pValues = new ArrayList<>();
        vValues = new ArrayList<>();
        pRealValues = new ArrayList<>();
        vRealValues = new ArrayList<>();
        pGlobalIDs = new ArrayList<>();
        vGlobalIDs = new ArrayList<>();
    }

    public List<String> getpTypes() {
        return pTypes;
    }
    public void setpTypes(List<String> pTypes) {
        this.pTypes = pTypes;
    }

    public List<String> getvTypes() {
        return vTypes;
    }
    public void setvTypes(List<String> vTypes) {
        this.vTypes = vTypes;
    }

    public List<Float> getpValues() {
        return pValues;
    }

    public void setpValues(List<Float> pValues) {
        this.pValues = pValues;
    }

    public List<Float> getvValues() {
        return vValues;
    }

    public void setvValues(List<Float> vValues) {
        this.vValues = vValues;
    }

    public List<Float> getpRealValues() {
        return pRealValues;
    }

    public void setpRealValues(List<Float> pRealValues) {
        this.pRealValues = pRealValues;
    }

    public List<Float> getvRealValues() {
        return vRealValues;
    }

    public void setvRealValues(List<Float> vRealValues) {
        this.vRealValues = vRealValues;
    }

    public List<Integer> getpGlobalIDs() {
        return pGlobalIDs;
    }

    public void setpGlobalIDs(List<Integer> pGlobalIDs) {
        this.pGlobalIDs = pGlobalIDs;
    }

    public List<Integer> getvGlobalIDs() {
        return vGlobalIDs;
    }

    public void setvGlobalIDs(List<Integer> vGlobalIDs) {
        this.vGlobalIDs = vGlobalIDs;
    }
}
